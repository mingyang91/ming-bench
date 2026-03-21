package ming

enum SchemeValue:
  case IntegerValue(value: BigInt)
  case BooleanValue(value: Boolean)
  case StringValue(value: String)

  def render: String = this match
    case SchemeValue.IntegerValue(value) => value.toString
    case SchemeValue.BooleanValue(value) => if value then "#t" else "#f"
    case SchemeValue.StringValue(value)  => "\"" + SchemeValue.escapeString(value) + "\""

object SchemeValue:

  def truthy(value: SchemeValue): Boolean = value match
    case SchemeValue.BooleanValue(false) => false
    case _                               => true

  private def escapeString(value: String): String =
    value.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case char => char.toString
    }
