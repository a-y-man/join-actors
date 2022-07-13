package actor

import java.util.concurrent.LinkedTransferQueue
import join_patterns.Matcher

abstract class Actor[M, T] extends Runnable {
  protected val q = LinkedTransferQueue[M]
  val ref         = ActorRef(q)
  protected val matcher: Matcher[M, T]
}

abstract class DynamicActor[M, T] extends Runnable {
  protected val q = LinkedTransferQueue[M]
  val ref         = ActorRef(q)
  protected var matcher: Matcher[M, T]
}
