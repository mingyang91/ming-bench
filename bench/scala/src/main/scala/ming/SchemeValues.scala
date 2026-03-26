package ming

sealed private[ming] trait Value:
  def render: String

final private[ming] case class IntValue(value: BigInt) extends Value:
  override def render: String = value.toString

final private[ming] case class BoolValue(value: Boolean) extends Value:
  override def render: String = if value then "#t" else "#f"

final private[ming] case class StringValue(value: String) extends Value:

  override def render: String =
    val escaped = value.flatMap:
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch   => ch.toString
    s"\"$escaped\""

final private[ming] case class SymbolValue(name: String) extends Value:
  override def render: String = name

private[ming] case object EmptyListValue extends Value:
  override def render: String = "()"

final private[ming] case class PairValue(car: Value, cdr: Value) extends Value:
  override def render: String = s"(${renderContents(this)})"

  private def renderContents(value: Value): String =
    value match
      case PairValue(head, EmptyListValue) =>
        head.render
      case PairValue(head, tail: PairValue) =>
        s"${head.render} ${renderContents(tail)}"
      case PairValue(head, tail) =>
        s"${head.render} . ${tail.render}"
      case other =>
        throw new IllegalStateException(s"expected pair while rendering pair, got ${other.render}")

final private[ming] case class BuiltinValue(
  name: String,
  implementation: (List[Value], Position) => Value
) extends Value:
  override def render: String = s"#<procedure:$name>"

final private[ming] case class ClosureValue(
  parameters: List[String],
  body: List[Expr],
  env: Environment,
  name: Option[String] = None
) extends Value:

  override def render: String =
    name match
      case Some(procedureName) => s"#<procedure:$procedureName>"
      case None                => "#<procedure:lambda>"

private[ming] case object VoidValue extends Value:
  override def render: String = "#<void>"

private[ming] case object UninitializedValue extends Value:
  override def render: String = "#<uninitialized>"
