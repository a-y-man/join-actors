package new_benchmarks.payment

class RefCell[T] private (private var _content: Option[T]):
  def this(initialContent: T) =
    this(Some(initialContent))

  def this() =
    this(None)

  def content_=(con: T): Unit =
    _content = Some(con)

  def content: T =
    _content.get
  
  def get: T = content
