package join_patterns.examples

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

sealed trait BoundedBuffer
case class Put(x: Int)           extends BoundedBuffer
case class Get(ref: ConsumerRef) extends BoundedBuffer
case class Terminate()           extends BoundedBuffer
// Internal events
case class Free(x: Int) extends BoundedBuffer
case class Full()       extends BoundedBuffer
// Response to Get event
case class Reply(x: Int) extends BoundedBuffer

type BBEvent       = Put | Get | Free | Full | Terminate
type ConsumerEvent = Reply | Terminate

type ConsumerRef = ActorRef[ConsumerEvent]
type BBRef       = ActorRef[BBEvent]

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

def bbActor(algorithm: MatchingAlgorithm): Actor[BBEvent, Unit] =
  Actor[BBEvent, Unit] {
    receive { (y: BBEvent, bbRef: BBRef) =>
      y match
        case (Put(s), Free(c)) if c == 1 =>
          // println(s"case 00: Put($s) & Free($c)")
          bbRef ! Full()
          bbRef ! Put(s)
          Next()
        case (Put(s), Free(c)) =>
          // println(s"case 01: Put($s) & Free($c)")
          bbRef ! Free(c - 1)
          bbRef ! Put(s)
          Next()
        case (Get(consumerRef), Put(x), Full()) =>
          // println(s"case 02: Get(consumerRef) & Put($x) & Full()")
          bbRef ! Free(1)
          consumerRef ! Reply(x)
          Next()
        case (Get(consumerRef), Put(x), Free(c)) =>
          // println(s"case 03: Get(consumerRef) & Put($x) & Free($c)")
          bbRef ! Free(c + 1)
          consumerRef ! Reply(x)
          Next()
        case Terminate() =>
          // println("case 04: BB terminated")
          Stop(())
    }(algorithm)
  }

def boundedBufferExample(algorithm: MatchingAlgorithm, bufferBound: Int) =
  val bb = bbActor(algorithm)

  val consumerActor = Actor[Reply | Terminate, Unit] {
    receive { (y: BoundedBuffer, selfRef: ConsumerRef) =>
      y match
        case Reply(x) =>
          println(s"Received: $x")
          Next()
        case Terminate() =>
          Stop(println("Consumer terminated"))
    }(algorithm)
  }

  val (_, bbRef)              = bb.start()
  val (replyFut, consumerRef) = consumerActor.start()

  bbRef ! Free(1)

  val producer = Future {
    (1 to bufferBound) foreach { i =>
      Thread.sleep(200)
      val p = Put(i)
      bbRef ! p
      println(s"$i -> $p")
      Thread.sleep(200)
    }
    // println("Producer terminated")
  }

  (1 to bufferBound) foreach { i =>
    Thread.sleep(200)
    val g = Get(consumerRef)
    bbRef ! g
    println(s"$i <- $g")
    Thread.sleep(200)
  }

  consumerRef ! Terminate()
  Await.ready(producer, Duration(1, TimeUnit.MINUTES))
  producer.onComplete(printResult)

  bbRef ! Terminate()
  Await.ready(replyFut, Duration(1, TimeUnit.MINUTES))
  replyFut.onComplete(printResult)
