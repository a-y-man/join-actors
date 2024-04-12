package benchmarks

import actor.*
import benchmarks.Benchmark
import benchmarks.BenchmarkPass
import benchmarks.Measurement
import benchmarks.saveToFile
import join_patterns.MatchingAlgorithm
import join_patterns.receive_

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

// Termination message to stop the actors
case class TerminateActors()

// Bounded buffer events
enum BoundedBuffer:
  case Put(ref: ProducerRef, x: Int)
  case Get(ref: ConsumerRef)

// Internal events
enum InternalEvent:
  case P(x: Int)
  case Free(x: Int)
  case Full()

// Response to Get event
enum ConsumerEvent:
  case CReply(bbRef: BBRef, x: Int)

// Response to Put event
enum ProducerEvent:
  case PReply(bbRef: BBRef)

type CEvent  = ConsumerEvent | TerminateActors
type PEvent  = ProducerEvent | TerminateActors
type BBEvent = BoundedBuffer | InternalEvent | ConsumerEvent | TerminateActors

type ProducerRef = ActorRef[ProducerEvent | TerminateActors]
type ConsumerRef = ActorRef[ConsumerEvent | TerminateActors]
type BBRef       = ActorRef[BBEvent]

case class BBConfig(
    val bufferBound: Int,
    val producers: Int,
    val consumers: Int,
    val cnt: Int,
    val algorithm: MatchingAlgorithm
)

def boundedBuffer(algorithm: MatchingAlgorithm): Actor[BBEvent, (Long, Int)] =
  import BoundedBuffer.*, InternalEvent.*, ConsumerEvent.*, ProducerEvent.*
  var matches = 0
  Actor[BBEvent, (Long, Int)] {
    receive_ { (bbRef: BBRef) =>
      {
        case (Put(producerRef, x), Free(c)) =>
          if c == 1 then bbRef ! Full()
          else bbRef ! Free(c - 1)
          bbRef ! P(x)
          producerRef ! PReply(bbRef)
          matches += 1
          Next()
        case (Get(consumerRef), P(x), Full()) =>
          bbRef ! Free(1)
          consumerRef ! CReply(bbRef, x)
          matches += 1
          Next()
        case (Get(consumerRef), P(x), Free(c)) =>
          bbRef ! Free(c + 1)
          consumerRef ! CReply(bbRef, x)
          matches += 1
          Next()
        case TerminateActors() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def consumer(bbRef: BBRef, maxCount: Int) =
  import BoundedBuffer.*, ConsumerEvent.*

  var cnt = 0
  Actor[CEvent, Unit] {
    receive_ { (selfRef: ConsumerRef) =>
      {
        case CReply(bbRef, x) if cnt < maxCount =>
          cnt += 1
          bbRef ! Get(selfRef)
          Next()
        case TerminateActors() if cnt == maxCount =>
          Stop(())
      }
    }(MatchingAlgorithm.BruteForceAlgorithm)
  }

def producer(bbRef: BBRef, maxCount: Int) =
  import BoundedBuffer.*, ProducerEvent.*

  var cnt = 0
  Actor[PEvent, Unit] {
    receive_ { (selfRef: ProducerRef) =>
      {
        case PReply(bbRef) if cnt < maxCount =>
          cnt += 1
          bbRef ! Put(selfRef, cnt)
          Next()
        case TerminateActors() if cnt == maxCount =>
          Stop(())
      }
    }(MatchingAlgorithm.BruteForceAlgorithm)
  }

def coordinator(
    bbRef: BBRef,
    prods: Array[(Future[Unit], ActorRef[PEvent])],
    cons: Array[(Future[Unit], ActorRef[CEvent])]
) =
  import BoundedBuffer.*, ConsumerEvent.*, ProducerEvent.*

  val prodsAndConsFut = Future.sequence(prods.map(_._1) ++ cons.map(_._1))

  val startTime = System.currentTimeMillis()

  Future {
    for (_, p) <- prods do
      bbRef ! Put(p, 0) // initial Put event to start the producer

      // This will only be consumed when the actor has
      // reached a maximum count of pre-configured value
      p ! TerminateActors()
  }

  Future {
    for (_, c) <- cons do
      bbRef ! Get(c) // initial Get event to start the consumer

      // This will only be consumed when the actor has
      // reached a maximum count of pre-configured value
      c ! TerminateActors()
  }

  // Wait for all producers and consumers to finish
  Await.ready(prodsAndConsFut, Duration(90, TimeUnit.MINUTES))

def measureBB(bbConfig: BBConfig) =
  import BoundedBuffer.*, InternalEvent.*, ConsumerEvent.*, ProducerEvent.*
  val bb             = boundedBuffer(bbConfig.algorithm)
  val (bbFut, bbRef) = bb.start()

  def startConsumers(bbRef: BBRef) =
    (1 to bbConfig.consumers).map(_ => consumer(bbRef, bbConfig.cnt).start()).toArray

  def startProds(bbRef: BBRef) =
    (1 to bbConfig.producers).map(_ => producer(bbRef, bbConfig.cnt).start()).toArray

  lazy val cons  = startConsumers(bbRef)
  lazy val prods = startProds(bbRef)

  Future {
    val startTime = System.currentTimeMillis()

    bbRef ! Free(bbConfig.bufferBound)
    coordinator(bbRef, prods, cons) // This blocks until all producers and consumers are done

    // Terminate the bounded buffer actor
    bbRef ! TerminateActors()

    val (endTime, finalMatchCount) = Await.result(bbFut, Duration.Inf)

    Measurement(endTime - startTime, finalMatchCount)
  }

def boundedBufferBenchmark(
    bbConfigs: Array[MatchingAlgorithm => BBConfig],
    algorithm: MatchingAlgorithm
) =
  val warmupConfig =
    BBConfig(
      bufferBound = 16,
      producers = 8,
      consumers = 8,
      cnt = 16,
      algorithm = algorithm
    )

  Benchmark(
    name = "Bounded Buffer",
    algorithm = algorithm,
    warmupRepititions = 5,
    repititons = 5,
    nullPass = BenchmarkPass(
      s"Null Pass ${algorithm}",
      () => measureBB(warmupConfig)
    ),
    passes = bbConfigs.map { config =>
      val bbConfig = config(algorithm)
      BenchmarkPass(
        s"${bbConfig.bufferBound} ${bbConfig.producers} ${bbConfig.consumers} ${bbConfig.cnt}",
        () => measureBB(bbConfig)
      )
    }
  )

def runBBBenchmark(bufferBound: Int, nProdsCons: Int, stepBy: Int, writeToFile: Boolean) =
  val bbConfigs =
    Array((1 to nProdsCons by stepBy).map(n => (bufferBound, n))*).map {
      case (bufferBound, nProdsCons) =>
        (algo: MatchingAlgorithm) =>
          BBConfig(
            bufferBound = bufferBound,
            producers = nProdsCons,
            consumers = nProdsCons,
            cnt = bufferBound,
            algorithm = algo
          )
    }

  val algorithms: List[MatchingAlgorithm] =
    List(MatchingAlgorithm.StatefulTreeBasedAlgorithm, MatchingAlgorithm.BruteForceAlgorithm)

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val m = boundedBufferBenchmark(bbConfigs, algorithm).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, m)
  }

  if writeToFile then saveToFile("BoundedBuffer", measurements)
