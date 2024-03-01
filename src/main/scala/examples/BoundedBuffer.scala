package join_patterns.examples

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import java.util.concurrent.Executors
import javax.management.Query
import scala.collection.immutable.LazyList.cons
import scala.collection.mutable.ListBuffer as Buffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

type ProducerRef = ActorRef[BoundedBuffer]
type ConsumerRef = ActorRef[Reply]

sealed trait BoundedBuffer
case class Put(ref: ProducerRef, x: Int) extends BoundedBuffer
case class Get(ref: ConsumerRef)         extends BoundedBuffer
case class Reply(x: Int)                 extends BoundedBuffer

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

def bbActor(bufferBound: Int, algorithm: MatchingAlgorithm): Actor[BoundedBuffer, Unit] =
  val buffer = Buffer[Int]()

  Actor[BoundedBuffer, Unit] {
    receive { (y: BoundedBuffer, selfRef: ActorRef[BoundedBuffer]) =>
      y match
        case (Get(consumerRef), Put(producerRef, x)) =>
          consumerRef ! Reply(x)
          Next()
        case Put(producerRef, x) =>
          if buffer.size < bufferBound then
            buffer.addOne(x)
            Next()
          else
            buffer.clear()
            buffer.addOne(x)
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

  Future(
    (1 to bufferBound) foreach { i =>
      bbRef ! Put(bbRef, i)
    }
  )

  Future(
    (1 to bufferBound) foreach { i =>
      bbRef ! Get(consumerRef)
    }
  )

  Await.ready(replyFut, Duration.Inf)

  replyFut.onComplete(printResult)
