package ming

/** AST / runtime value for the Scheme interpreter. */
enum SchemeValue:
  case IntVal(value: Long, pos: Option[SourcePos] = None)
  case BoolVal(value: Boolean, pos: Option[SourcePos] = None)
  case StringVal(value: String, pos: Option[SourcePos] = None)
  case SymbolVal(name: String, pos: Option[SourcePos] = None)
  case ListVal(elements: List[SchemeValue], pos: Option[SourcePos] = None)
  case PairVal(car: SchemeValue, cdr: SchemeValue)
  case LambdaVal(params: List[String], body: List[SchemeValue], closure: Environment)
  case BuiltinVal(name: String, func: List[SchemeValue] => SchemeValue)
  case Void

  /** Get the source position of this value, if any. */
  def sourcePos: Option[SourcePos] = this match
    case IntVal(_, p)    => p
    case BoolVal(_, p)   => p
    case StringVal(_, p) => p
    case SymbolVal(_, p) => p
    case ListVal(_, p)   => p
    case _               => None

  def display: String = this match
    case IntVal(n, _)       => n.toString
    case BoolVal(b, _)      => if b then "#t" else "#f"
    case StringVal(s, _)    => s"\"$s\""
    case SymbolVal(n, _)    => n
    case ListVal(es, _)     => s"(${es.map(_.display).mkString(" ")})"
    case PairVal(_, _)      => displayPair(this)
    case LambdaVal(_, _, _) => "#<procedure>"
    case BuiltinVal(n, _)   => s"#<builtin:$n>"
    case Void               => ""

  def isTruthy: Boolean = this match
    case BoolVal(false, _) => false
    case _                 => true

  private def displayPair(p: SchemeValue): String =
    val parts   = scala.collection.mutable.ListBuffer[String]()
    var current = p
    var done    = false
    while !done do
      current match
        case PairVal(car, cdr) =>
          parts += car.display
          current = cdr
        case ListVal(Nil, _) =>
          done = true
        case ListVal(es, _) =>
          parts ++= es.map(_.display)
          done = true
        case other =>
          parts += "."
          parts += other.display
          done = true
    s"(${parts.mkString(" ")})"
