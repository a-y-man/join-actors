package join_patterns

import scala.collection.mutable.ListBuffer

import java.util.concurrent.LinkedTransferQueue as Queue

class Matcher[M, T](val patterns: List[JoinPattern[M, T]]) {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages = ListBuffer[M]()

  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    while (result.isEmpty)
      for
        pattern <- patterns
        if result.isEmpty
      do
        if messages.size >= pattern.size then
          val (matchedMessages, substs) = pattern.extract(messages.toList)

          if matchedMessages.nonEmpty && pattern.guard(substs) then
            result = Some(pattern.rhs(substs))
            messages.subtractAll(matchedMessages)

      if result.isEmpty then
        // If we arrive here, no pattern has been matched and we need more msgs
        messages.append(q.take())
        q.drainTo(messages)

    result.get
}
