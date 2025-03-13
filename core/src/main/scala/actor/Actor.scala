package join_actors.actor

import join_patterns.matching.Matcher

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

enum Result[+T]:
  case Stop(value: T)
  case Continue

import Result.*

/** Represents an actor that processes messages of type M and produces a result of type T.
  *
  * @param matcher
  *   A matcher is the object that performs the join pattern matching on the messages in the actor's
  *   mailbox.
  * @tparam M
  *   The type of messages processed by the actor.
  * @tparam T
  *   The type of result produced by the actor. Which is the right-hand side of the join pattern.
  */
class Actor[M, T](private val matcher: Matcher[M, Result[T]]):
  private val mailbox: Mailbox[M] = Mailbox[M]
  private val self                = ActorRef(mailbox)

  /** Starts the actor and returns a future that will be completed with the result produced by the
    * actor, and the actor reference.
    *
    * @return
    *   A tuple containing the future result and the actor reference.
    */
  def start(): (Future[T], ActorRef[M]) =
    val promise = Promise[T]

    ec.execute(() => run(promise))

    (promise.future, self)

  /** Runs the actor's message processing loop recursively until a stop signal is received, and
    * completes the provided promise with the resulting value.
    *
    * @param promise
    *   The promise to be completed with the actor's result.
    */
  @tailrec
  private def run(promise: Promise[T]): Unit =
    matcher(mailbox)(self) match
      case Continue    => run(promise)
      case Stop(value) => promise.success(value)
