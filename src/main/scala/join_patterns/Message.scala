package join_patterns

/**
 * Intended to use with `System.currentTimeMillis`
 * */
abstract class Message {
	val _timestamp = System.currentTimeMillis

	override def toString: String = s"[$_timestamp] ${this.getClass.getSimpleName}"
}
