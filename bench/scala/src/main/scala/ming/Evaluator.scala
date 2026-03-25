package ming

/** Scheme interpreter entry point. */
object Evaluator:

  def evalStr(input: String): String =
    val tokens            = Tokenizer.tokenize(input)
    val exprs             = Parser.parseAll(tokens)
    var result: SchemeVal = SchemeVal.Void
    val env               = Env.default()
    for expr <- exprs do result = eval(expr, env)
    SchemeVal.display(result)

  def evalStrWithOutput(input: String): (String, String) =
    throw new EvalError("not implemented")

  def eval(expr: SchemeVal, env: Env): SchemeVal = expr match
    case SchemeVal.IntVal(n)    => expr
    case SchemeVal.BoolVal(b)   => expr
    case SchemeVal.StringVal(s) => expr
    case SchemeVal.Symbol(name) =>
      env.lookup(name) match
        case Some(v) => v
        case None    => throw new EvalError(s"unbound variable: $name")
    case SchemeVal.SList(elems) if elems.isEmpty =>
      throw new EvalError("empty application")
    case SchemeVal.SList(elems) =>
      val head = elems.head
      head match
        case SchemeVal.Symbol("and") => evalAnd(elems.tail, env)
        case SchemeVal.Symbol("or")  => evalOr(elems.tail, env)
        case _ =>
          val proc = eval(head, env)
          val args = elems.tail.map(a => eval(a, env))
          apply(proc, args)
    case _ => throw new EvalError(s"cannot evaluate: $expr")

  private def evalAnd(exprs: List[SchemeVal], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVal.BoolVal(true)
    else
      var result: SchemeVal = SchemeVal.BoolVal(true)
      for e <- exprs do
        result = eval(e, env)
        if !isTruthy(result) then return result
      result

  private def evalOr(exprs: List[SchemeVal], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVal.BoolVal(false)
    else
      for e <- exprs do
        val result = eval(e, env)
        if isTruthy(result) then return result
      SchemeVal.BoolVal(false)

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  def apply(proc: SchemeVal, args: List[SchemeVal]): SchemeVal = proc match
    case SchemeVal.BuiltinProc(_, f) => f(args)
    case _                           => throw new EvalError(s"not a procedure: $proc")

enum SchemeVal:
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case StringVal(s: String)
  case Symbol(name: String)
  case SList(elems: List[SchemeVal])
  case Void
  case BuiltinProc(name: String, f: List[SchemeVal] => SchemeVal)

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

class Env(private val bindings: scala.collection.mutable.Map[String, SchemeVal], private val parent: Option[Env]):

  def lookup(name: String): Option[SchemeVal] =
    bindings.get(name).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

object Env:

  def default(): Env =
    val env = new Env(scala.collection.mutable.Map.empty, None)
    Builtins.register(env)
    env

object Builtins:

  private def requireNums(args: List[SchemeVal], name: String): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other               => throw new EvalError(s"$name: expected number, got ${SchemeVal.display(other)}")
    }

  def register(env: Env): Unit =
    env.define(
      "+",
      SchemeVal.BuiltinProc(
        "+",
        args =>
          val nums = requireNums(args, "+")
          SchemeVal.IntVal(nums.sum)
      )
    )
    env.define(
      "-",
      SchemeVal.BuiltinProc(
        "-",
        args =>
          val nums = requireNums(args, "-")
          if nums.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if nums.size == 1 then SchemeVal.IntVal(-nums.head)
          else SchemeVal.IntVal(nums.tail.foldLeft(nums.head)(_ - _))
      )
    )
    env.define(
      "*",
      SchemeVal.BuiltinProc(
        "*",
        args =>
          val nums = requireNums(args, "*")
          SchemeVal.IntVal(nums.product)
      )
    )
    env.define(
      "/",
      SchemeVal.BuiltinProc(
        "/",
        args =>
          val nums = requireNums(args, "/")
          if nums.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          if nums.tail.contains(0L) then throw new EvalError("division by zero")
          SchemeVal.IntVal(nums.tail.foldLeft(nums.head)(_ / _))
      )
    )
    env.define(
      "<",
      SchemeVal.BuiltinProc(
        "<",
        args =>
          val nums = requireNums(args, "<")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a < b))
      )
    )
    env.define(
      ">",
      SchemeVal.BuiltinProc(
        ">",
        args =>
          val nums = requireNums(args, ">")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a > b))
      )
    )
    env.define(
      "=",
      SchemeVal.BuiltinProc(
        "=",
        args =>
          val nums = requireNums(args, "=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a == b))
      )
    )
    env.define(
      "<=",
      SchemeVal.BuiltinProc(
        "<=",
        args =>
          val nums = requireNums(args, "<=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a <= b))
      )
    )
    env.define(
      ">=",
      SchemeVal.BuiltinProc(
        ">=",
        args =>
          val nums = requireNums(args, ">=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a >= b))
      )
    )
    env.define(
      "not",
      SchemeVal.BuiltinProc(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          args.head match
            case SchemeVal.BoolVal(false) => SchemeVal.BoolVal(true)
            case _                        => SchemeVal.BoolVal(false)
      )
    )
