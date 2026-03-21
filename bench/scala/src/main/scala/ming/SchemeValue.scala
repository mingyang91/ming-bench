package ming

enum SchemeValue:
  case IntegerValue(value: BigInt)
  case BooleanValue(value: Boolean)
  case StringValue(value: String)
  case SymbolValue(value: String)
  case EmptyListValue
  case PairValue(car: SchemeValue, cdr: SchemeValue)
  case ProcedureValue(procedure: SchemeProcedure)
  case VoidValue

  def render: String = this match
    case SchemeValue.IntegerValue(value) => value.toString
    case SchemeValue.BooleanValue(value) => if value then "#t" else "#f"
    case SchemeValue.StringValue(value)  => "\"" + SchemeValue.escapeString(value) + "\""
    case SchemeValue.SymbolValue(value)  => value
    case SchemeValue.EmptyListValue      => "()"
    case SchemeValue.PairValue(car, cdr) => "(" + SchemeValue.renderPairContents(car, cdr) + ")"
    case SchemeValue.ProcedureValue(_)   => "#<procedure>"
    case SchemeValue.VoidValue           => "#<void>"

object SchemeValue:

  def truthy(value: SchemeValue): Boolean = value match
    case SchemeValue.BooleanValue(false) => false
    case _                               => true

  def list(values: List[SchemeValue]): SchemeValue =
    values.foldRight(SchemeValue.EmptyListValue: SchemeValue)(SchemeValue.PairValue(_, _))

  private def escapeString(value: String): String =
    value.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case char => char.toString
    }

  private def renderPairContents(car: SchemeValue, cdr: SchemeValue): String =
    cdr match
      case SchemeValue.EmptyListValue        => car.render
      case SchemeValue.PairValue(next, rest) => s"${car.render} ${renderPairContents(next, rest)}"
      case other                             => s"${car.render} . ${other.render}"
