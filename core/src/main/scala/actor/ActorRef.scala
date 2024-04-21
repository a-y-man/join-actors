package actor

import java.util.concurrent.LinkedTransferQueue

/** Represents a reference to an actor.
  *
  * @param q
  *   The underlying queue used for message passing. This is a blocking queue that blocks its thread
  *   when the queue is empty.
  * @tparam M
  *   The type of messages that can be sent to the actor.
  */
class ActorRef[-M](q: LinkedTransferQueue[M]):
  /** Sends a message to the actor.
    *
    * @param m
    *   The message to be sent.
    */
  def send(m: M): Unit = q.put(m)

  /** Sends a message to the actor using the `!` operator.
    *
    * @param m
    *   The message to be sent.
    */
  def !(m: M): Unit = send(m)
