package ming

enum SchemeValue:
  case IntVal(value: Long)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case CharVal(value: Char)
  case SymbolVal(name: String, pos: Option[(Int, Int)] = None)
  case ListVal(elements: List[SchemeValue], pos: Option[(Int, Int)] = None)
  case MutableStringVal(chars: Array[Char])
  case PairVal(car: SchemeValue, cdr: SchemeValue)

  case LambdaVal(
    params: List[String],
    body: List[SchemeValue],
    closure: Map[String, SchemeValue],
    selfName: Option[String] = None
  )
  case Void

  /** write-style display (strings get quotes) — used for evalStr results. */
  def display: String = this match
    case IntVal(n)            => n.toString
    case BoolVal(b)           => if b then "#t" else "#f"
    case StringVal(s)         => "\"" + s + "\""
    case MutableStringVal(cs) => "\"" + String(cs) + "\""
    case CharVal(c)           => s"#\\$c"
    case SymbolVal(n, _)      => n
    case ListVal(es, _)       => "(" + es.map(_.display).mkString(" ") + ")"
    case p: PairVal           => "(" + displayPairInner(p) + ")"
    case _: LambdaVal         => "#<procedure>"
    case Void                 => ""

  /** display-style output (strings without quotes). */
  def displayOut: String = this match
    case StringVal(s)         => s
    case MutableStringVal(cs) => String(cs)
    case CharVal(c)           => c.toString
    case other                => other.display

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true

  private def displayPairInner(p: PairVal): String =
    p.cdr match
      case ListVal(Nil, _) => p.car.display
      case p2: PairVal     => p.car.display + " " + displayPairInner(p2)
      case ListVal(es, _)  => p.car.display + " " + es.map(_.display).mkString(" ")
      case other           => p.car.display + " . " + other.display
