package join_patterns

import java.util.concurrent.LinkedTransferQueue

class ActorRef[M](q: LinkedTransferQueue[M]) {
  def send(m: M) = q.put(m)
  def !(m: M)    = send(m)
}
