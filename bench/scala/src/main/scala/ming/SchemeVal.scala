package ming

case class Pos(line: Int, col: Int)

/** Scheme value types */
enum SchemeVal:
  var pos: Option[Pos] = None
  case SInt(value: Long)
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

  /** Write representation (with quotes for strings). */
  def display: String = this match
    case SInt(v)    => v.toString
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

  /** Display representation (no quotes for strings). */
  def displayRepr: String = this match
    case SString(v) => v.toString
    case SChar(c)   => c.toString
    case other      => other.display
