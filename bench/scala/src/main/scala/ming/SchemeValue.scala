package ming

/** AST / runtime value for the Scheme interpreter. */
enum SchemeValue:
  case IntVal(value: Long, pos: Option[SourcePos] = None)
  case RationalVal(num: Long, den: Long, pos: Option[SourcePos] = None)
  case DoubleVal(value: Double, pos: Option[SourcePos] = None)
  case BoolVal(value: Boolean, pos: Option[SourcePos] = None)
  case StringVal(value: String, pos: Option[SourcePos] = None)
  case SymbolVal(name: String, pos: Option[SourcePos] = None)
  case ListVal(elements: List[SchemeValue], pos: Option[SourcePos] = None)
  case PairVal(car: SchemeValue, cdr: SchemeValue)
  case LambdaVal(params: List[String], restParam: Option[String], body: List[SchemeValue], closure: Environment)
  case CharVal(value: Char, pos: Option[SourcePos] = None)
  case MutableStringVal(chars: Array[Char], pos: Option[SourcePos] = None)
  case VectorVal(elements: Array[SchemeValue], pos: Option[SourcePos] = None)
  case BuiltinVal(name: String, func: List[SchemeValue] => SchemeValue)

  case ContinuationVal(
    contId: Long,
    bodyExprs: List[SchemeValue],
    bodyEnv: Environment,
    contextStack: List[BodyContext],
    seqRemaining: List[SchemeValue],
    seqEnv: Environment,
    hasSameBodyFrame: Boolean
  )
  case SyntaxRulesVal(name: String, literals: Set[String], rules: List[(SchemeValue, SchemeValue)], defEnv: Environment)
  case ValuesVal(values: List[SchemeValue])
  case RecordVal(typeId: Long, typeName: String, fields: Array[SchemeValue])
  case Void

  /** Get the source position of this value, if any. */
  def sourcePos: Option[SourcePos] = this match
    case IntVal(_, p)           => p
    case RationalVal(_, _, p)   => p
    case DoubleVal(_, p)        => p
    case BoolVal(_, p)          => p
    case StringVal(_, p)        => p
    case SymbolVal(_, p)        => p
    case ListVal(_, p)          => p
    case CharVal(_, p)          => p
    case MutableStringVal(_, p) => p
    case VectorVal(_, p)        => p
    case _                      => None

  /** Format for `write` — strings are quoted. */
  def display: String = this match
    case IntVal(n, _)                         => n.toString
    case RationalVal(n, d, _)                 => s"$n/$d"
    case DoubleVal(d, _)                      => Rational.formatDouble(d)
    case BoolVal(b, _)                        => if b then "#t" else "#f"
    case StringVal(s, _)                      => s"\"$s\""
    case MutableStringVal(cs, _)              => s"\"${String(cs)}\""
    case SymbolVal(n, _)                      => n
    case CharVal(c, _)                        => s"#\\$c"
    case VectorVal(es, _)                     => s"#(${es.map(_.display).mkString(" ")})"
    case ListVal(es, _)                       => s"(${es.map(_.display).mkString(" ")})"
    case PairVal(_, _)                        => displayPair(this)
    case LambdaVal(_, _, _, _)                => "#<procedure>"
    case BuiltinVal(n, _)                     => s"#<builtin:$n>"
    case ContinuationVal(_, _, _, _, _, _, _) => "#<continuation>"
    case SyntaxRulesVal(_, _, _, _)           => "#<macro>"
    case ValuesVal(vs)                        => vs.map(_.display).mkString(" ")
    case RecordVal(_, tn, fs)                 => s"#<record:$tn>"
    case Void                                 => ""

  /** Format for `display` — strings are unquoted. */
  def displayOutput: String = this match
    case StringVal(s, _)         => s
    case MutableStringVal(cs, _) => String(cs)
    case CharVal(c, _)           => c.toString
    case _                       => display

  /** Format for display inside a list (used by displayPair). */
  private def displayListElement: String = display

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
