package join_actors.actor

import join_patterns.matching.Matcher

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.*
import scala.util.control.NonFatal

/** Runs each actor's message loop on its own virtual thread. Deliberately not an implicit
  * `ExecutionContext`: user code chooses its own execution context for its futures.
  */
private val actorExecutor: ExecutorService = Executors.newVirtualThreadPerTaskExecutor()

/** Runs `loop` on its own virtual thread and completes the returned future with its result. If
  * `loop` throws, the future fails with that exception instead of never completing. `cleanup` runs
  * in both cases.
  */
private def runActorLoop[T](loop: () => T, cleanup: () => Unit): Future[T] =
  val promise = Promise[T]()
  actorExecutor.execute { () =>
    try promise.success(loop())
    catch
      case e: Throwable =>
        promise.tryFailure(e)
        if !NonFatal(e) then throw e
    finally cleanup()
  }
  promise.future

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
  private val self = ActorRef(mailbox)

  /** Starts the actor and returns a future that will be completed with the result produced by the
    * actor, and the actor reference.
    *
    * The future fails if a join pattern (its guard or right-hand side) throws an exception; the
    * actor then stops. Once the actor stops, its matcher is closed.
    *
    * @return
    *   A tuple containing the future result and the actor reference.
    */
  def start(): (Future[T], ActorRef[M]) =
    (runActorLoop(() => run(), () => matcher.close()), self)

  /** Runs the actor's message processing loop recursively until a stop signal is received, and
    * returns the resulting value.
    */
  @tailrec
  private def run(): T =
    matcher(mailbox)(self) match
      case Continue => run()
      case Stop(value) => value

/** A simple actor implementation that processes messages of type M and produces a result of type T.
  *
  * This actor does not use the receive macro and directly uses regular Scala partial functions for
  * message handling.
  *
  * @param f
  *   A function that takes an ActorRef and returns a partial function for message handling.
  * @tparam M
  *   The type of messages this actor can receive.
  * @tparam T
  *   The type of the final result produced when the actor stops.
  */
class SimpleActor[M, T](private val f: ActorRef[M] => PartialFunction[Any, Result[T]]):

  private val mailbox: Mailbox[M] = Mailbox[M]
  private val self = ActorRef(mailbox)

  /** Starts the actor. As for [[Actor.start]], the future fails if the handler throws. */
  def start(): (Future[T], ActorRef[M]) =
    (runActorLoop(() => run(), () => ()), self)

  @tailrec
  private def run(): T =
    f(self).applyOrElse[M, Result[T]](mailbox.take(), _ => Continue) match
      case Continue => run()
      case Stop(value) => value
