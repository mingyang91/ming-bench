package ming

/** Scheme value representation. */
enum Value:
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StringVal(s: String)
  case CharVal(c: Char)
  case Symbol(name: String, pos: Option[(Int, Int)] = None)
  case PairVal(car: Value, cdr: Value, pos: Option[(Int, Int)] = None)
  case NilVal

  case LambdaVal(
    params: List[String],
    body: List[Value],
    closure: () => Env,
    name: Option[String],
    restParam: Option[String] = None
  )
  case MutableStringVal(chars: Array[Char])
  case VoidVal

  case ContinuationVal(
    tag: AnyRef,
    remaining: List[Value],
    envThunk: () => Env,
    capturedOut: String
  )

  /** Extract string content from either StringVal or MutableStringVal. */
  def stringContent: Option[String] = this match
    case StringVal(s)         => Some(s)
    case MutableStringVal(cs) => Some(String(cs))
    case _                    => None

  /** Write representation (strings with quotes). */
  def display: String = this match
    case IntVal(n)            => n.toString
    case BoolVal(b)           => if b then "#t" else "#f"
    case StringVal(s)         => "\"" + s + "\""
    case MutableStringVal(cs) => "\"" + String(cs) + "\""
    case CharVal(c)           => s"#\\$c"
    case Symbol(name, _)      => name
    case NilVal               => "()"
    case PairVal(_, _, _)     => formatList(_.display)
    case _: LambdaVal         => "#<procedure>"
    case _: ContinuationVal   => "#<continuation>"
    case VoidVal              => ""

  /** Display representation (strings without quotes). */
  def displayRepr: String = this match
    case StringVal(s)         => s
    case MutableStringVal(cs) => String(cs)
    case CharVal(c)           => c.toString
    case PairVal(_, _, _)     => formatList(_.displayRepr)
    case other                => other.display

  private def formatList(fmt: Value => String): String =
    val (elems, tail) = collectList(this, List.empty)
    tail match
      case NilVal =>
        "(" + elems.map(fmt).mkString(" ") + ")"
      case other =>
        "(" + elems.map(fmt).mkString(" ") + " . " + fmt(other) + ")"

  private def collectList(
    v: Value,
    acc: List[Value]
  ): (List[Value], Value) =
    v match
      case PairVal(car, cdr, _) => collectList(cdr, acc :+ car)
      case other                => (acc, other)
