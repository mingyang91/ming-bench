package ming

enum SchemeVal:
  var pos: (Int, Int) = (0, 0)
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StringVal(s: String)
  case Symbol(name: String)
  case SList(elems: List[SchemeVal])
  case Void
  case BuiltinProc(name: String, f: List[SchemeVal] => SchemeVal)
  case LambdaProc(params: List[String], body: List[SchemeVal], closure: Env)

object SchemeVal:

  def display(v: SchemeVal): String = v match
    case IntVal(n)            => n.toString
    case BoolVal(true)        => "#t"
    case BoolVal(false)       => "#f"
    case StringVal(s)         => s"\"$s\""
    case Symbol(name)         => name
    case SList(elems)         => "(" + elems.map(display).mkString(" ") + ")"
    case Void                 => ""
    case BuiltinProc(name, _) => s"#<procedure $name>"
    case LambdaProc(_, _, _)  => "#<procedure>"

class Env(
  private val bindings: scala.collection.mutable.Map[String, SchemeVal],
  private val parent: Option[Env]
):

  def lookup(name: String): Option[SchemeVal] =
    bindings.get(name).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

object Env:

  def default(): Env =
    val env = new Env(scala.collection.mutable.Map.empty, None)
    Builtins.register(env)
    env
