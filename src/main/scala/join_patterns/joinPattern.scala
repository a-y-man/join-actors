package join_patterns


/** An ADT defintion of a join pattern
  *
  * */
case class JoinPattern[M, T](
    extract: List[M] => (List[M], Map[String, Any]),
    guard: Map[String, Any] => Boolean,
    rhs: Map[String, Any] => T,
    size: Int
)