package ming

/** Scheme value representation. */
enum Value:
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StringVal(s: String)
  case Symbol(name: String)
  case PairVal(car: Value, cdr: Value)
  case NilVal

  def display: String = this match
    case IntVal(n)     => n.toString
    case BoolVal(b)    => if b then "#t" else "#f"
    case StringVal(s)  => "\"" + s + "\""
    case Symbol(name)  => name
    case NilVal        => "()"
    case PairVal(_, _) => displayList(this)

  private def displayList(v: Value): String =
    val (elems, tail) = collectList(v, List.empty)
    tail match
      case NilVal => "(" + elems.map(_.display).mkString(" ") + ")"
      case other  => "(" + elems.map(_.display).mkString(" ") + " . " + other.display + ")"

  private def collectList(v: Value, acc: List[Value]): (List[Value], Value) =
    v match
      case PairVal(car, cdr) => collectList(cdr, acc :+ car)
      case other             => (acc, other)
