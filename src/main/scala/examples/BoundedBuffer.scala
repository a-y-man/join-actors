package join_patterns.examples

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import javax.management.Query
import scala.collection.immutable.LazyList.cons
import scala.collection.mutable.ArrayBuffer as Buffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

type ConsumerRef = ActorRef[Reply]

sealed trait BoundedBuffer
case class Put(x: Int)           extends BoundedBuffer
case class Get(ref: ConsumerRef) extends BoundedBuffer
case class Reply(x: Int)         extends BoundedBuffer

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

def bbActor(bufferBound: Int, algorithm: MatchingAlgorithm): Actor[BoundedBuffer, String] =
  val buffer = Buffer[Int](bufferBound)

  Actor[BoundedBuffer, String] {
    receive { (y: BoundedBuffer, selfRef: ActorRef[BoundedBuffer]) =>
      y match
        case (Get(consumerRef), Put(x)) =>
          consumerRef ! Reply(x)
          Next()
        case Put(x) =>
          if buffer.size < bufferBound then
            buffer.addOne(x)
            Next()
          else
            println(s"Buffer is full, cannot add $x")
            Next()
    }(algorithm)
  }

def boundedBufferExample(algorithm: MatchingAlgorithm, bufferBound: Int) =
  val bb = bbActor(bufferBound, algorithm)

  val consumerActor = Actor[Reply, Unit] {
    receive { (y: BoundedBuffer, selfRef: ActorRef[Reply]) =>
      y match
        case Reply(x) =>
          println(s"Received: $x")
          Next()
    }(MatchingAlgorithm.BruteForceAlgorithm)
  }

  val (_, bbRef)              = bb.start()
  val (replyFut, consumerRef) = consumerActor.start()

  val producer = Future(
    (1 to bufferBound) foreach { i =>
      Thread.sleep(200)
      bbRef ! Put(i)
      Thread.sleep(200)
    }
  )

  (1 to bufferBound) foreach { i =>
    Thread.sleep(200)
    bbRef ! Get(consumerRef)
    Thread.sleep(200)
  }

  Await.ready(producer, Duration(1, TimeUnit.MINUTES))
  producer.onComplete(printResult)

  Await.ready(replyFut, Duration(1, TimeUnit.MINUTES))
  replyFut.onComplete(printResult)
