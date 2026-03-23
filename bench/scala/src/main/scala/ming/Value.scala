package ming

/** Runtime Scheme value. */
enum Value:
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StrVal(chars: Array[Char])
  case PairVal(car: Value, cdr: Value)
  case NilVal
  case SymbolVal(name: String)
  case LambdaVal(params: List[String], body: List[Expr], closure: Env)
  case BuiltinVal(name: String, fn: List[Value] => Value)
  case CharVal(c: Char)

  def display: String = this match
    case IntVal(n)           => n.toString
    case BoolVal(b)          => if b then "#t" else "#f"
    case StrVal(chars)       => s"\"${new String(chars)}\""
    case NilVal              => "()"
    case SymbolVal(name)     => name
    case PairVal(_, _)       => displayList(this)
    case LambdaVal(_, _, _)  => "#<procedure>"
    case BuiltinVal(name, _) => s"#<procedure:$name>"
    case CharVal(c)          => s"#\\$c"

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
