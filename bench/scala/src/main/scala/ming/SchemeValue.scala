package ming

/** AST / runtime value for the Scheme interpreter. */
enum SchemeValue:
  case IntVal(value: Long)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case SymbolVal(name: String)
  case ListVal(elements: List[SchemeValue])
  case PairVal(car: SchemeValue, cdr: SchemeValue)
  case LambdaVal(params: List[String], body: List[SchemeValue], closure: Environment)
  case BuiltinVal(name: String, func: List[SchemeValue] => SchemeValue)
  case Void

  def display: String = this match
    case IntVal(n)          => n.toString
    case BoolVal(b)         => if b then "#t" else "#f"
    case StringVal(s)       => s"\"$s\""
    case SymbolVal(n)       => n
    case ListVal(es)        => s"(${es.map(_.display).mkString(" ")})"
    case PairVal(_, _)      => displayPair(this)
    case LambdaVal(_, _, _) => "#<procedure>"
    case BuiltinVal(n, _)   => s"#<builtin:$n>"
    case Void               => ""

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true

  private def displayPair(p: SchemeValue): String =
    val parts   = scala.collection.mutable.ListBuffer[String]()
    var current = p
    var done    = false
    while !done do
      current match
        case PairVal(car, cdr) =>
          parts += car.display
          current = cdr
        case ListVal(Nil) =>
          done = true
        case ListVal(es) =>
          parts ++= es.map(_.display)
          done = true
        case other =>
          parts += "."
          parts += other.display
          done = true
    s"(${parts.mkString(" ")})"
