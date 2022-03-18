package join_patterns

abstract class Message {
	val _timestamp = System.currentTimeMillis

	override def toString: String = s"[$_timestamp] ${this.getClass.getSimpleName}"
}
