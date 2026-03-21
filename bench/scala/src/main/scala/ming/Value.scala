package ming

/** Scheme value representation. */
enum Value:
  case IntVal(n: Long)
  case RationalVal(num: Long, den: Long)
  case DoubleVal(d: Double)
  case BoolVal(b: Boolean)
  case StringVal(s: String)
  case CharVal(c: Char)
  case Symbol(name: String, pos: Option[(Int, Int)] = None)
  case PairVal(car: Value, cdr: Value, pos: Option[(Int, Int)] = None)
  case NilVal

  case LambdaVal(
    params: List[String],
    body: List[Value],
    closure: () => Env,
    name: Option[String],
    restParam: Option[String] = None
  )
  case MutableStringVal(chars: Array[Char])
  case MutablePairVal(cell: Array[Value])
  case VoidVal

  case ContinuationVal(
    tag: AnyRef,
    remaining: List[Value],
    envThunk: () => Env,
    capturedOut: String,
    bodyLevel: Boolean = false,
    windEntries: List[(Value, Value)] = Nil
  )

  case MacroVal(
    rules: List[(Value, Value)],
    literals: List[String],
    defEnv: () => Env
  )

  case VectorVal(elems: Array[Value])

  case MultipleValues(vals: List[Value])

  case RecordVal(typeTag: AnyRef, fieldNames: List[String], fields: Array[Value])

  case NativeProcVal(name: String, fn: List[Value] => Value)

  /** Extract string content from either StringVal or MutableStringVal. */
  def stringContent: Option[String] = this match
    case StringVal(s)         => Some(s)
    case MutableStringVal(cs) => Some(String(cs))
    case _                    => None

  /** Write representation (strings with quotes). */
  def display: String = this match
    case IntVal(n)             => n.toString
    case RationalVal(num, den) => s"$num/$den"
    case DoubleVal(d)          => formatDouble(d)
    case BoolVal(b)            => if b then "#t" else "#f"
    case StringVal(s)          => "\"" + s + "\""
    case MutableStringVal(cs)  => "\"" + String(cs) + "\""
    case CharVal(c)            => s"#\\$c"
    case Symbol(name, _)       => name
    case NilVal                => "()"
    case PairVal(_, _, _)      => formatList(_.display)
    case _: MutablePairVal     => formatList(_.display)
    case VectorVal(elems) =>
      "#(" + elems.map(_.display).mkString(" ") + ")"
    case _: LambdaVal       => "#<procedure>"
    case _: NativeProcVal   => "#<procedure>"
    case _: ContinuationVal => "#<continuation>"
    case _: MacroVal        => "#<macro>"
    case _: RecordVal       => "#<record>"
    case VoidVal            => ""
    case MultipleValues(vs) => vs.map(_.display).mkString(" ")

  /** Display representation (strings without quotes). */
  def displayRepr: String = this match
    case StringVal(s)         => s
    case MutableStringVal(cs) => String(cs)
    case CharVal(c)           => c.toString
    case VectorVal(elems) =>
      "#(" + elems.map(_.displayRepr).mkString(" ") + ")"
    case PairVal(_, _, _)  => formatList(_.displayRepr)
    case _: MutablePairVal => formatList(_.displayRepr)
    case other             => other.display

  private def formatList(fmt: Value => String): String =
    val (elems, tail) = collectList(this, List.empty)
    tail match
      case NilVal =>
        "(" + elems.map(fmt).mkString(" ") + ")"
      case other =>
        "(" + elems.map(fmt).mkString(" ") + " . " + fmt(other) + ")"

  private def formatDouble(d: Double): String =
    if d == d.toLong.toDouble && !d.isInfinite then
      val s = d.toString
      if s.contains('.') then s else s + ".0"
    else d.toString

  private def collectList(
    v: Value,
    acc: List[Value],
    seen: Set[AnyRef] = Set.empty
  ): (List[Value], Value) =
    v match
      case MutablePairVal(cell) =>
        if seen.contains(cell) then (acc, Symbol("..."))
        else collectList(cell(1), acc :+ cell(0), seen + cell)
      case PairVal(car, cdr, _) => collectList(cdr, acc :+ car, seen)
      case other                => (acc, other)

object Value:

  /** Create a rational, simplifying to IntVal when denominator is 1. */
  def makeRational(num: Long, den: Long): Value =
    assert(den != 0, "denominator must not be zero")
    val sign = if den < 0 then -1L else 1L
    val n    = num * sign
    val d    = den * sign
    val g    = gcd(math.abs(n), d)
    val sn   = n / g
    val sd   = d / g
    if sd == 1L then IntVal(sn) else RationalVal(sn, sd)

  @scala.annotation.tailrec
  private def gcd(a: Long, b: Long): Long =
    if b == 0L then a else gcd(b, a % b)
