package ming

/** Runtime Scheme value. */
enum Value:
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StrVal(s: String)

  def display: String = this match
    case IntVal(n)  => n.toString
    case BoolVal(b) => if b then "#t" else "#f"
    case StrVal(s)  => s"\"$s\""

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true
