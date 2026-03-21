package ming

/** AST / runtime values for the Scheme interpreter. */
enum Value:
  case Integer(n: Long)
  case Bool(b: Boolean)
  case Str(s: String)
  case MutStr(chars: Array[scala.Char])
  case Symbol(name: String)
  case SList(elems: List[Value])

  case Lambda(
    params: List[String],
    body: List[Value],
    closure: Env,
    selfName: Option[String] = None,
    restParam: Option[String] = None
  )
  case Char(c: scala.Char)
  case Rational(num: Long, den: Long)
  case Float(d: Double)
  case Continuation(id: Long)
  case Macro(literals: List[String], rules: List[(Value, Value)], defEnv: Env)
  case TransformerMacro(transformer: Value, defEnv: Env)
  case Pair(cell: Array[Value])
  case Vec(elems: Array[Value])
  case Void
  case Values(vals: List[Value])
  case Record(tag: String, fields: List[Value])
  case RecordConstructor(tag: String, fieldCount: Int)
  case RecordPredicate(tag: String)
  case RecordAccessor(tag: String, fieldIndex: Int)

  /** Write format — strings are quoted, chars use #\ syntax. */
  def display: String = this match
    case Integer(n)           => n.toString
    case Rational(num, den)   => s"$num/$den"
    case Float(d)             => formatFloat(d)
    case Bool(b)              => if b then "#t" else "#f"
    case Str(s)               => s"\"$s\""
    case MutStr(chars)        => s"\"${new String(chars)}\""
    case Symbol(n)            => n
    case SList(Nil)           => "()"
    case SList(elems)         => elems.map(_.display).mkString("(", " ", ")")
    case _: Lambda            => "#<procedure>"
    case _: Continuation      => "#<continuation>"
    case _: Macro             => "#<macro>"
    case _: TransformerMacro  => "#<macro>"
    case _: Record            => "#<record>"
    case _: RecordConstructor => "#<procedure>"
    case _: RecordPredicate   => "#<procedure>"
    case _: RecordAccessor    => "#<procedure>"
    case p: Pair              => displayPairChain(p, _.display)
    case Vec(elems)           => elems.map(_.display).mkString("#(", " ", ")")
    case Char(c)              => s"#\\$c"
    case Void                 => "#<void>"
    case Values(vals)         => vals.map(_.display).mkString(" ")

  private def formatFloat(d: Double): String =
    val s = d.toString
    if s.contains('.') || s.contains('E') || s.contains('e') then s
    else s + ".0"

  /** Display format — strings unquoted, chars as bare character. */
  def displayStr: String = this match
    case Str(s)        => s
    case MutStr(chars) => new String(chars)
    case Char(c)       => c.toString
    case SList(Nil)    => "()"
    case SList(elems)  => elems.map(_.displayStr).mkString("(", " ", ")")
    case p: Pair       => displayPairChain(p, _.displayStr)
    case Vec(elems)    => elems.map(_.displayStr).mkString("#(", " ", ")")
    case other         => other.display

  private def displayPairChain(start: Pair, fmt: Value => String): String =
    @scala.annotation.tailrec
    def collect(cur: Value, acc: List[String]): String = cur match
      case Pair(c)    => collect(c(1), acc :+ fmt(c(0)))
      case SList(Nil) => acc.mkString("(", " ", ")")
      case other      => (acc :+ "." :+ fmt(other)).mkString("(", " ", ")")
    collect(start.cell(1), List(fmt(start.cell(0))))

/** Environment with lexical scoping, mutable cells, and optional shared top-level cell. */
case class Env(
  bindings: Map[String, Array[Value]],
  parent: Option[Env],
  shared: Option[Array[Map[String, Value]]] = None
):

  def lookup(name: String): Value =
    bindings.get(name) match
      case Some(cell) => cell(0)
      case None =>
        shared
          .flatMap(cell => cell(0).get(name))
          .getOrElse(
            parent match
              case Some(p) => p.lookup(name)
              case None    => throw new EvalError(s"unbound variable: $name")
          )

  def set(name: String, value: Value): Unit =
    bindings.get(name) match
      case Some(cell) => cell(0) = value
      case None =>
        shared match
          case Some(s) if s(0).contains(name) =>
            s(0) = s(0) + (name -> value)
          case _ =>
            parent match
              case Some(p) => p.set(name, value)
              case None    => throw new EvalError(s"unbound variable: $name")

  def extend(name: String, value: Value): Env =
    Env(bindings + (name -> Array(value)), Some(this))

  def extendAll(names: List[String], values: List[Value]): Env =
    Env(bindings ++ names.zip(values).map((n, v) => n -> Array(v)).toMap, Some(this))

object Value:

  def isFalsy(v: Value): Boolean = v match
    case Value.Bool(false) => true
    case _                 => false

object Env:
  val empty: Env = Env(Map.empty, None)
