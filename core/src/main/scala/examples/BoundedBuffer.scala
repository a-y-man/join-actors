package join_patterns.examples

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
import join_patterns.receiveOld

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

// Termination message to stop the actors
case class Terminate()

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

type CEvent  = ConsumerEvent | Terminate
type PEvent  = ProducerEvent | Terminate
type BBEvent = BoundedBuffer | InternalEvent | ConsumerEvent | Terminate

type ProducerRef = ActorRef[ProducerEvent | Terminate]
type ConsumerRef = ActorRef[ConsumerEvent | Terminate]
type BBRef       = ActorRef[BBEvent]

case class BBConfig(
    val bufferBound: Int,
    val producers: Int,
    val consumers: Int,
    val cnt: Int,
    val algorithm: MatchingAlgorithm
)

def boundedBuffer(algorithm: MatchingAlgorithm): Actor[BBEvent, Long] =
  import BoundedBuffer.*, InternalEvent.*, ConsumerEvent.*, ProducerEvent.*
  Actor[BBEvent, Long] {
    receive { (bbRef: BBRef) =>
      {
        case (Put(producerRef, x), Free(c)) =>
          if c == 1 then bbRef ! Full()
          else bbRef ! Free(c - 1)
          bbRef ! P(x)
          producerRef ! PReply(bbRef)
          Continue()
        case (Get(consumerRef), P(x), Full()) =>
          bbRef ! Free(1)
          consumerRef ! CReply(bbRef, x)
          Continue()
        case (Get(consumerRef), P(x), Free(c)) =>
          bbRef ! Free(c + 1)
          consumerRef ! CReply(bbRef, x)
          Continue()
        case Terminate() =>
          Stop(System.currentTimeMillis())
      }
    }(algorithm)
  }

def consumer(bbRef: BBRef, maxCount: Int) =
  import BoundedBuffer.*, ConsumerEvent.*

  var cnt = 0
  Actor[CEvent, Unit] {
    receive { (selfRef: ConsumerRef) =>
      {
        case CReply(bbRef, x) if cnt < maxCount =>
          println(s"Actor: $selfRef -- Received: $x")
          cnt += 1
          bbRef ! Get(selfRef)
          Continue()
        case Terminate() if cnt == maxCount =>
          Stop(())
      }
    }(MatchingAlgorithm.BruteForceAlgorithm)
  }

def producer(bbRef: BBRef, maxCount: Int) =
  import BoundedBuffer.*, ProducerEvent.*

  var cnt = 0
  Actor[PEvent, Unit] {
    receive { (selfRef: ProducerRef) =>
      {
        case PReply(bbRef) if cnt < maxCount =>
          println(s"Actor: $selfRef -- Sent: $cnt")
          cnt += 1
          bbRef ! Put(selfRef, cnt)
          Continue()
        case Terminate() if cnt == maxCount =>
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

  Future {
    for (_, p) <- prods do
      println(s"Producer: $p")
      bbRef ! Put(p, 0)
      p ! Terminate()
  }

  Future {
    for (_, c) <- cons do
      println(s"Consumer: $c")
      bbRef ! Get(c)
      c ! Terminate()
  }

  Await.ready(prodsAndConsFut, Duration(90, TimeUnit.MINUTES))
  bbRef ! Terminate()

def runBB(bbConfig: BBConfig) =
  import BoundedBuffer.*, InternalEvent.*, ConsumerEvent.*, ProducerEvent.*
  val bb = boundedBuffer(bbConfig.algorithm)

  val (bbFut, bbRef) = bb.start()
  bbRef ! Free(bbConfig.bufferBound)

  def startConsumers(bbRef: BBRef) =
    (0 until bbConfig.consumers).map(_ => consumer(bbRef, bbConfig.cnt).start()).toArray

  def startProds(bbRef: BBRef) =
    (0 until bbConfig.producers).map(_ => producer(bbRef, bbConfig.cnt).start()).toArray

  lazy val cons  = startConsumers(bbRef)
  lazy val prods = startProds(bbRef)

  val startTime = System.currentTimeMillis()

  coordinator(bbRef, prods, cons)

  val endTime = Await.result(bbFut, Duration.Inf)

  println(s"${bbConfig.algorithm}: ${endTime - startTime} ms")
