package old_benchmarks

import old_benchmarks.Benchmark
import old_benchmarks.BenchmarkPass
import old_benchmarks.Measurement
import old_benchmarks.saveToFile
import join_actors.api.*
import os.Path

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.Duration

// Termination message to stop the actors
case class TerminateActors()

type BufferType = String

// Bounded buffer events
enum BoundedBuffer:
  case Put(ref: ProducerSyncReply, x: BufferType)
  case Get(ref: ConsumerReply)

// Internal events
enum InternalEvent:
  case P(x: BufferType)
  case Free(x: Int)
  case Full()

// Response to Get event
type ConsumerReply = Promise[BufferType]

// Response to Put event
type ProducerSyncReply = Promise[Unit]

type BBEvent = BoundedBuffer | InternalEvent | ConsumerReply | TerminateActors

type BBRef = ActorRef[BBEvent]

case class BBConfig(
    val bufferBound: Int,
    val producers: Int,
    val consumers: Int,
    val cnt: Int,
    val matcher: MatcherFactory
)

def boundedBuffer(matcher: MatcherFactory): Actor[BBEvent, (Long, Int)] =
  import BoundedBuffer.*, InternalEvent.*
  var matches = 0
  Actor[BBEvent, (Long, Int)] {
    receive { (bbRef: BBRef) =>
      {
        case (Put(producerRef, x), Free(c)) =>
          if c == 1 then bbRef ! Full()
          else bbRef ! Free(c - 1)
          bbRef ! P(x)
          producerRef.success(())
          Continue
        case (Get(consumerRef), P(x), Full()) =>
          bbRef ! Free(1)
          consumerRef.success(x)
          matches += 1
          Continue
        case (Get(consumerRef), P(x), Free(c)) =>
          bbRef ! Free(c + 1)
          consumerRef.success(x)
          matches += 1
          Continue
        case TerminateActors() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(matcher)
  }

def coordinator(
    bbRef: BBRef,
    bbConfig: BBConfig
) =
  import BoundedBuffer.*

  val msg = "hello"

  def producer() =
    Future {
      for i <- 0 until bbConfig.cnt do
        val prodPromise: ProducerSyncReply = Promise[Unit]()
        val prodFut = prodPromise.future
        bbRef ! Put(prodPromise, msg)
        Await.ready(prodFut, Duration(10, TimeUnit.MINUTES))
    }

  def consumer() =
    Future {
      for _ <- 0 until bbConfig.cnt do
        val consPromise: ConsumerReply = Promise[BufferType]()
        val consFut = consPromise.future
        bbRef ! Get(consPromise)
        Await.ready(consFut, Duration(10, TimeUnit.MINUTES))
    }

  // Create and start consumers
  val consumers = Future.sequence((0 until bbConfig.consumers).map(_ => consumer()))

  // Create and start producers
  val producers = Future.sequence((0 until bbConfig.producers).map(_ => producer()))

  Await.ready(Future.sequence(Seq(consumers, producers)), Duration(10, TimeUnit.MINUTES))

  bbRef ! TerminateActors()

def measureBB(bbConfig: BBConfig) =
  import BoundedBuffer.*, InternalEvent.*
  val bb = boundedBuffer(bbConfig.matcher)
  val (bbFut, bbRef) = bb.start()

  Future {
    val startTime = System.currentTimeMillis()

    bbRef ! Free(bbConfig.bufferBound)
    coordinator(
      bbRef,
      bbConfig
    ) // This blocks until all producers and consumers are done

    // Terminate the bounded buffer actor
    bbRef ! TerminateActors()

    val (endTime, finalMatchCount) = Await.result(bbFut, Duration(10, TimeUnit.MINUTES))

    Measurement(endTime - startTime, finalMatchCount)
  }

def boundedBufferBenchmark(
    bbConfigs: Array[MatcherFactory => BBConfig],
    matcher: MatcherFactory,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =
  val warmupConfig =
    BBConfig(
      bufferBound = 16,
      producers = 8,
      consumers = 8,
      cnt = 16,
      matcher = matcher
    )

  Benchmark(
    name = "Bounded Buffer",
    matcher = matcher,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      s"Null Pass ${matcher}",
      () => measureBB(warmupConfig)
    ),
    passes = bbConfigs.map { config =>
      val bbConfig = config(matcher)
      BenchmarkPass(
        s"${bbConfig.bufferBound} ${bbConfig.producers} ${bbConfig.consumers} ${bbConfig.cnt}",
        () => measureBB(bbConfig)
      )
    }
  )

def runBBBenchmark(
    bufferBound: Int,
    nProdsCons: Int,
    writeToFile: Boolean,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val bbConfigs =
    Array((1 to nProdsCons).map(n => (bufferBound, n))*).map { case (bufferBound, nProdsCons) =>
      (matcher: MatcherFactory) =>
        BBConfig(
          bufferBound = bufferBound,
          producers = nProdsCons,
          consumers = nProdsCons,
          cnt = bufferBound,
          matcher = matcher
        )
    }

  val matchers: List[MatcherFactory] =
    List(
      StatefulTreeMatcher,
      MutableStatefulMatcher,
      LazyMutableMatcher,
      WhileLazyMatcher,
      FilteringWhileMatcher,
      WhileEagerMatcher,
      ArrayWhileMatcher,
      EagerParallelMatcher(2),
      EagerParallelMatcher(4),
      EagerParallelMatcher(6),
      EagerParallelMatcher(8),
      LazyParallelMatcher(2),
      LazyParallelMatcher(4),
      LazyParallelMatcher(6),
      LazyParallelMatcher(8),
      FilteringParallelMatcher(2),
      FilteringParallelMatcher(4),
      FilteringParallelMatcher(6),
      FilteringParallelMatcher(8),
      ArrayParallelMatcher(2),
      ArrayParallelMatcher(4),
      ArrayParallelMatcher(6),
      ArrayParallelMatcher(8)
    )

  val measurements = matchers map { matcher =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $matcher${Console.RESET}"
    )
    val m = boundedBufferBenchmark(bbConfigs, matcher, warmupRepetitions, repetitions).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $matcher finished${Console.RESET}"
    )

    (matcher, m)
  }

  if writeToFile then saveToFile("BoundedBuffer", measurements, outputDataDir)
