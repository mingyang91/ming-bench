package ming

enum Value:
  case Number(value: BigInt)
  case Bool(value: Boolean)
  case Str(value: String)

  def isTruthy: Boolean = this match
    case Value.Bool(false) => false
    case _                 => true

  def render: String = this match
    case Value.Number(value) => value.toString
    case Value.Bool(value)   => if value then "#t" else "#f"
    case Value.Str(value)    => "\"" + escape(value) + "\""

  private def escape(value: String): String =
    value.flatMap:
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case char => char.toString
