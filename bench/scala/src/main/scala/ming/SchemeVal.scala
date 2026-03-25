package ming

case class Pos(line: Int, col: Int)

/** Scheme value types */
enum SchemeVal:
  var pos: Option[Pos] = None
  case SInt(value: Long)
  case SFloat(value: Double)
  case SRational(num: Long, den: Long)
  case SBool(value: Boolean)
  case SString(value: StringBuilder)
  case SSymbol(name: String)
  case SChar(value: Char)
  case SList(elems: List[SchemeVal])
  case SPair(car: SchemeVal, cdr: SchemeVal)
  case SVoid

  case SLambda(
    params: List[String],
    restParam: Option[String],
    body: List[SchemeVal],
    closure: Env
  )

  case SMacro(
    literals: Set[String],
    clauses: List[(SchemeVal, SchemeVal)],
    defEnv: Env
  )

  /** Write representation (with quotes for strings). */
  def display: String = this match
    case SInt(v) => v.toString
    case SFloat(v) =>
      if v == v.toLong.toDouble && !v.isInfinite then f"$v%.1f"
      else v.toString
    case SRational(n, d) =>
      if d == 1 then n.toString else s"$n/$d"
    case SBool(v)   => if v then "#t" else "#f"
    case SString(v) => s""""${v.toString}""""
    case SSymbol(n) => n
    case SChar(c) =>
      c match
        case ' '  => "#\\space"
        case '\n' => "#\\newline"
        case '\t' => "#\\tab"
        case _    => s"#\\$c"
    case SList(es) => "(" + es.map(_.display).mkString(" ") + ")"
    case SPair(a, d) =>
      def pairTail(v: SchemeVal): String = v match
        case SList(Nil)    => ""
        case SList(es)     => " " + es.map(_.display).mkString(" ")
        case SPair(ca, cd) => " " + ca.display + pairTail(cd)
        case other         => " . " + other.display
      "(" + a.display + pairTail(d) + ")"
    case SVoid               => ""
    case SLambda(_, _, _, _) => "#<procedure>"
    case SMacro(_, _, _)     => "#<macro>"

  /** Display representation (no quotes for strings). */
  def displayRepr: String = this match
    case SString(v) => v.toString
    case SChar(c)   => c.toString
    case other      => other.display

object SchemeVal:

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a.abs else gcd(b, a % b)

  /** Create a rational, normalizing sign and reducing to lowest terms. Returns SInt if denominator is 1.
    */
  def makeRational(num: Long, den: Long): SchemeVal =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1L else 1L
    val n    = num * sign
    val d    = den * sign
    val g    = gcd(n.abs, d)
    val rn   = n / g
    val rd   = d / g
    if rd == 1 then SInt(rn) else SRational(rn, rd)
