package ming

enum SchemeValue:
  case IntVal(value: Long)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case SymbolVal(name: String)
  case ListVal(elements: List[SchemeValue])
  case PairVal(car: SchemeValue, cdr: SchemeValue)

  case LambdaVal(
    params: List[String],
    body: List[SchemeValue],
    closure: Map[String, SchemeValue],
    selfName: Option[String] = None
  )
  case Void

  def display: String = this match
    case IntVal(n)    => n.toString
    case BoolVal(b)   => if b then "#t" else "#f"
    case StringVal(s) => "\"" + s + "\""
    case SymbolVal(n) => n
    case ListVal(es)  => "(" + es.map(_.display).mkString(" ") + ")"
    case p: PairVal   => "(" + displayPairInner(p) + ")"
    case _: LambdaVal => "#<procedure>"
    case Void         => ""

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true

  private def displayPairInner(p: PairVal): String =
    p.cdr match
      case ListVal(Nil) => p.car.display
      case p2: PairVal  => p.car.display + " " + displayPairInner(p2)
      case ListVal(es)  => p.car.display + " " + es.map(_.display).mkString(" ")
      case other        => p.car.display + " . " + other.display
