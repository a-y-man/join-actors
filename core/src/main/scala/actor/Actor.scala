package actor

import join_patterns.Matcher

import java.util.concurrent.Executors
import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.*

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

sealed trait Result[T]
case class Stop[T](value: T) extends Result[T]
case class Next[T]()         extends Result[T]

class Actor[M, T](private val matcher: Matcher[M, Result[T]]):
  private val mailbox: Mailbox[M] = Mailbox[M]
  val self                        = ActorRef(mailbox)

  def start(): (Future[T], ActorRef[M]) =
    val promise = Promise[T]

    ec.execute(() => run(promise))

    (promise.future, self)

  @tailrec
  private def run(promise: Promise[T]): Unit =
    matcher(mailbox)(self) match
      case Next()      => run(promise)
      case Stop(value) => promise.success(value)
