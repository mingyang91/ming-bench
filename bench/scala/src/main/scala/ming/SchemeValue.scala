package ming

enum SchemeValue:
  case IntVal(value: Long)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case CharVal(value: Char)
  case SymbolVal(name: String, pos: Option[(Int, Int)] = None)
  case ListVal(elements: List[SchemeValue], pos: Option[(Int, Int)] = None)
  case MutableStringVal(chars: Array[Char])
  case VectorVal(elements: Array[SchemeValue])
  case PairVal(car: SchemeValue, cdr: SchemeValue)
  case MutablePairVal(cell: Array[SchemeValue])

  case LambdaVal(
    params: List[String],
    body: List[SchemeValue],
    closure: Map[String, SchemeValue],
    selfName: Option[String] = None,
    restParam: Option[String] = None
  )
  case Cell(content: Array[SchemeValue])
  case ContinuationVal(k: (SchemeValue, Map[String, SchemeValue]) => Bounce)

  case MacroVal(
    literals: List[String],
    rules: List[(List[SchemeValue], SchemeValue)],
    defEnv: Map[String, SchemeValue]
  )
  case Void

  /** write-style display (strings get quotes) — used for evalStr results. */
  def display: String = this match
    case IntVal(n)            => n.toString
    case BoolVal(b)           => if b then "#t" else "#f"
    case StringVal(s)         => "\"" + s + "\""
    case MutableStringVal(cs) => "\"" + String(cs) + "\""
    case VectorVal(es)        => "#(" + es.map(_.display).mkString(" ") + ")"
    case CharVal(c)           => s"#\\$c"
    case SymbolVal(n, _)      => n
    case ListVal(es, _)       => "(" + es.map(_.display).mkString(" ") + ")"
    case AnyPair(_, _)        => "(" + displayAnyPairInner(this) + ")"
    case _: LambdaVal         => "#<procedure>"
    case _: ContinuationVal   => "#<continuation>"
    case _: MacroVal          => "#<macro>"
    case Cell(arr)            => arr(0).display
    case Void                 => ""

  /** display-style output (strings without quotes). */
  def displayOut: String = this match
    case StringVal(s)         => s
    case MutableStringVal(cs) => String(cs)
    case CharVal(c)           => c.toString
    case VectorVal(es)        => "#(" + es.map(_.display).mkString(" ") + ")"
    case Cell(arr)            => arr(0).displayOut
    case other                => other.display

  def isTruthy: Boolean = this match
    case BoolVal(false)     => false
    case Cell(arr)          => arr(0).isTruthy
    case _: ContinuationVal => true
    case _                  => true

  private def displayAnyPairInner(p: SchemeValue): String =
    val (hd, tl) = p match
      case PairVal(a, b)        => (a, b)
      case MutablePairVal(data) => (data(0), data(1))
      case _                    => return p.display
    tl match
      case ListVal(Nil, _) => hd.display
      case AnyPair(_, _)   => hd.display + " " + displayAnyPairInner(tl)
      case ListVal(es, _)  => hd.display + " " + es.map(_.display).mkString(" ")
      case other           => hd.display + " . " + other.display

/** Extractor for both PairVal and MutablePairVal. */
object AnyPair:
  def unapply(v: SchemeValue): Option[(SchemeValue, SchemeValue)] = v match
    case SchemeValue.PairVal(a, b)        => Some((a, b))
    case SchemeValue.MutablePairVal(data) => Some((data(0), data(1)))
    case _                                => None
