package ming

object Value:

  /** Identity set tracking which char arrays are mutable (from string-copy). */
  private val mutableStrings: java.util.Set[Array[Char]] =
    java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]())

  def markStringMutable(chars: Array[Char]): Unit  = mutableStrings.add(chars)
  def isStringMutable(chars: Array[Char]): Boolean = mutableStrings.contains(chars)

  private def gcd(a: Long, b: Long): Long =
    val aa = Math.abs(a)
    val bb = Math.abs(b)
    if bb == 0 then aa else gcd(bb, aa % bb)

  /** Create a normalized rational or integer value. */
  def makeRational(num: Long, den: Long): Value =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1L else 1L
    val g    = gcd(Math.abs(num), Math.abs(den))
    val n    = sign * num / g
    val d    = sign * den / g
    if d == 1L then Value.IntVal(n)
    else Value.RationalVal(n, d)

/** Runtime Scheme value. */
enum Value:
  case IntVal(n: Long)
  case RationalVal(num: Long, den: Long)
  case FloatVal(d: Double)
  case BoolVal(b: Boolean)
  case StrVal(chars: Array[Char])
  case PairVal(cell: PairCell)
  case NilVal
  case SymbolVal(name: String)
  case LambdaVal(params: List[String], restParam: Option[String], body: List[Expr], closure: Env)
  case BuiltinVal(name: String, fn: List[Value] => Value)
  case CharVal(c: Char)
  case ContinuationVal(k: Value => Bounce)
  case MacroVal(literals: List[String], rules: List[(List[Expr], Expr)], defEnv: Env)
  case VectorVal(elems: Array[Value])
  case VoidVal
  case ValuesVal(values: List[Value])
  case RecordVal(tag: String, fields: Array[Value])
  case ProcMacroVal(transformer: Value, defEnv: Env)
  case CaseLambdaVal(clauses: List[(List[String], Option[String], List[Expr])], closure: Env)

  def display: String = this match
    case IntVal(n)             => n.toString
    case RationalVal(n, d)     => s"$n/$d"
    case FloatVal(d)           => formatFloat(d)
    case BoolVal(b)            => if b then "#t" else "#f"
    case StrVal(chars)         => s"\"${new String(chars)}\""
    case NilVal                => "()"
    case SymbolVal(name)       => name
    case PairVal(_)            => displayList(this)
    case LambdaVal(_, _, _, _) => "#<procedure>"
    case BuiltinVal(name, _)   => s"#<procedure:$name>"
    case CharVal(c)            => s"#\\$c"
    case ContinuationVal(_)    => "#<continuation>"
    case MacroVal(_, _, _)     => "#<macro>"
    case VectorVal(elems)      => elems.map(_.display).mkString("#(", " ", ")")
    case VoidVal               => "#<void>"
    case ValuesVal(_)          => "#<values>"
    case RecordVal(tag, _)     => s"#<record:$tag>"
    case ProcMacroVal(_, _)    => "#<macro>"
    case CaseLambdaVal(_, _)   => "#<procedure>"

  /** Display without quotes (for `display` builtin). */
  def displayNoQuotes: String = this match
    case StrVal(chars) => new String(chars)
    case other         => other.display

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true

  private def formatFloat(d: Double): String =
    if d == d.toLong.toDouble && !d.isInfinite then s"${d.toLong}.0"
    else d.toString

  private def displayList(v: Value): String =
    val sb = new StringBuilder("(")
    val seen = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[PairCell, java.lang.Boolean]()
    )
    var current = v
    var first   = true
    while current match
        case PairVal(cell) =>
          if seen.contains(cell) then
            if !first then sb.append(" ")
            sb.append("...")
            false
          else
            seen.add(cell)
            if !first then sb.append(" ")
            first = false
            sb.append(cell.car.display)
            current = cell.cdr
            true
        case NilVal =>
          false
        case other =>
          sb.append(" . ").append(other.display)
          false
    do ()
    sb.append(")").toString
