package actor

import java.util.concurrent.LinkedTransferQueue
import scala.annotation.targetName

class ActorRef[-M](q: LinkedTransferQueue[M]):
  def send(m: M): Unit = q.put(m)

  def !(m: M): Unit = send(m)
