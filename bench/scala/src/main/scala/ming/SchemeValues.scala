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

final private[ming] case class BuiltinValue(
  name: String,
  implementation: (List[Value], Position) => Value
) extends Value:
  override def render: String = s"#<procedure:$name>"
