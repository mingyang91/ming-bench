package ming

enum SchemeVal:
  var pos: (Int, Int) = (0, 0)
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StringVal(chars: Array[Char])
  case Symbol(name: String)
  case CharVal(c: Char)
  case SList(elems: List[SchemeVal])
  case DottedList(elems: List[SchemeVal], tail: SchemeVal)
  case Void
  case BuiltinProc(name: String, f: List[SchemeVal] => SchemeVal)
  case LambdaProc(params: List[String], body: List[SchemeVal], closure: Env, rest: Option[String] = None)

object SchemeVal:

  def str(s: String): StringVal = StringVal(s.toCharArray)

  /** write-style display (strings quoted) */
  def display(v: SchemeVal): String = v match
    case IntVal(n)               => n.toString
    case BoolVal(true)           => "#t"
    case BoolVal(false)          => "#f"
    case StringVal(chars)        => s"\"${new String(chars)}\""
    case CharVal(' ')            => "#\\space"
    case CharVal('\n')           => "#\\newline"
    case CharVal('\t')           => "#\\tab"
    case CharVal(c)              => s"#\\$c"
    case Symbol(name)            => name
    case SList(elems)            => "(" + elems.map(display).mkString(" ") + ")"
    case DottedList(elems, tail) => "(" + elems.map(display).mkString(" ") + " . " + display(tail) + ")"
    case Void                    => ""
    case BuiltinProc(name, _)    => s"#<procedure $name>"
    case LambdaProc(_, _, _, _)  => "#<procedure>"

  /** display-style output (strings unquoted) */
  def displayOutput(v: SchemeVal): String = v match
    case StringVal(chars)        => new String(chars)
    case CharVal(c)              => c.toString
    case SList(elems)            => "(" + elems.map(displayOutput).mkString(" ") + ")"
    case DottedList(elems, tail) => "(" + elems.map(displayOutput).mkString(" ") + " . " + displayOutput(tail) + ")"
    case other                   => display(other)

  def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (IntVal(x), IntVal(y))       => x == y
    case (BoolVal(x), BoolVal(y))     => x == y
    case (StringVal(x), StringVal(y)) => java.util.Arrays.equals(x, y)
    case (CharVal(x), CharVal(y))     => x == y
    case (Symbol(x), Symbol(y))       => x == y
    case (SList(xs), SList(ys))       => xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b))
    case (DottedList(xs, xt), DottedList(ys, yt)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b)) && schemeEqual(xt, yt)
    case (Void, Void) => true
    case _            => false

class Env(
  private val bindings: scala.collection.mutable.Map[String, SchemeVal],
  private val parent: Option[Env]
):

  def lookup(name: String): Option[SchemeVal] =
    bindings.get(name).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

  def set(name: String, value: SchemeVal): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value)
        case None    => throw new EvalError(s"set!: unbound variable: $name")

object Env:

  def default(): Env =
    val env = new Env(scala.collection.mutable.Map.empty, None)
    Builtins.register(env)
    env

  def defaultWithOutput(output: StringBuilder): Env =
    val env = new Env(scala.collection.mutable.Map.empty, None)
    Builtins.register(env, output)
    env
