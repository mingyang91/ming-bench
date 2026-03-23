package ming

object Value:
  /** Identity set tracking which char arrays are mutable (from string-copy). */
  private val mutableStrings: java.util.Set[Array[Char]] =
    java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]())

  def markStringMutable(chars: Array[Char]): Unit = mutableStrings.add(chars)
  def isStringMutable(chars: Array[Char]): Boolean = mutableStrings.contains(chars)

/** Runtime Scheme value. */
enum Value:
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StrVal(chars: Array[Char])
  case PairVal(car: Value, cdr: Value)
  case NilVal
  case SymbolVal(name: String)
  case LambdaVal(params: List[String], restParam: Option[String], body: List[Expr], closure: Env)
  case BuiltinVal(name: String, fn: List[Value] => Value)
  case CharVal(c: Char)
  case ContinuationVal(k: Value => Bounce)
  case MacroVal(literals: List[String], rules: List[(List[Expr], Expr)], defEnv: Env)

  def display: String = this match
    case IntVal(n)             => n.toString
    case BoolVal(b)            => if b then "#t" else "#f"
    case StrVal(chars)         => s"\"${new String(chars)}\""
    case NilVal                => "()"
    case SymbolVal(name)       => name
    case PairVal(_, _)         => displayList(this)
    case LambdaVal(_, _, _, _) => "#<procedure>"
    case BuiltinVal(name, _)   => s"#<procedure:$name>"
    case CharVal(c)            => s"#\\$c"
    case ContinuationVal(_)    => "#<continuation>"
    case MacroVal(_, _, _)     => "#<macro>"

  /** Display without quotes (for `display` builtin). */
  def displayNoQuotes: String = this match
    case StrVal(chars) => new String(chars)
    case other         => other.display

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true

  private def displayList(v: Value): String =
    val sb      = new StringBuilder("(")
    var current = v
    var first   = true
    while current match
        case PairVal(car, cdr) =>
          if !first then sb.append(" ")
          first = false
          sb.append(car.display)
          current = cdr
          true
        case NilVal =>
          false
        case other =>
          sb.append(" . ").append(other.display)
          false
    do ()
    sb.append(")").toString
