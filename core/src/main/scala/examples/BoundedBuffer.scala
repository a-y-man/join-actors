package join_actors.examples

import join_actors.api.*

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.Duration

// Termination message to stop the actors
case class Terminate()

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

type BBEvent = BoundedBuffer | InternalEvent | ConsumerReply | Terminate

type BBRef = ActorRef[BBEvent]

case class BBConfig(
    val bufferBound: Int,
    val producers: Int,
    val consumers: Int,
    val cnt: Int,
    val algorithm: MatcherFactory
)

def boundedBuffer(matcher: MatcherFactory): Actor[BBEvent, Long] =
  import BoundedBuffer.*, InternalEvent.*
  var matches = 0
  var puts = 0
  var gets = 0
  Actor[BBEvent, Long] {
    receive[BBEvent, Long] { (bbRef: BBRef) =>
      {
        case Put(producerRef, x) &:& Free(c) =>
          if c == 1 then bbRef ! Full()
          else bbRef ! Free(c - 1)
          bbRef ! P(x)
          producerRef.success(())
          puts += 1
          Continue
        case Get(consumerRef) &:& P(x) &:& Full() =>
          bbRef ! Free(1)
          consumerRef.success(x)
          gets += 1
          matches += 1
          Continue
        case Get(consumerRef) &:& P(x) &:& Free(c) =>
          bbRef ! Free(c + 1)
          consumerRef.success(x)
          gets += 1
          matches += 1
          Continue
        case Terminate() =>
          // println(s"Matches: $matches, Puts: $puts, Gets: $gets")
          Stop(System.currentTimeMillis())
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
        Await.ready(prodFut, Duration(10, TimeUnit.SECONDS))
    }

  def consumer() =
    Future {
      for _ <- 0 until bbConfig.cnt do
        val consPromise: ConsumerReply = Promise[BufferType]()
        val consFut = consPromise.future
        bbRef ! Get(consPromise)
        val got = Await.result(consFut, Duration(10, TimeUnit.SECONDS))
        // println(s"Got: $got")
    }

  // Create and start consumers
  val consumers = Future.sequence((0 until bbConfig.consumers).map(_ => consumer()))

  // Create and start producers
  val producers = Future.sequence((0 until bbConfig.producers).map(_ => producer()))

  Await.ready(Future.sequence(Seq(consumers, producers)), Duration(10, TimeUnit.SECONDS))

  bbRef ! Terminate()

def runBB(bbConfig: BBConfig) =
  import BoundedBuffer.*, InternalEvent.*
  val bb = boundedBuffer(bbConfig.algorithm)

  val (bbFut, bbRef) = bb.start()
  bbRef ! Free(bbConfig.bufferBound)

  val startTime = System.currentTimeMillis()

  coordinator(bbRef, bbConfig)

  val endTime = Await.result(bbFut, Duration.Inf)

  println(s"${bbConfig.algorithm}: ${endTime - startTime} ms")
