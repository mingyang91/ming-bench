package ming

import scala.collection.mutable

private[ming] object SchemeInterpreter:

  sealed trait Expr

  object Expr:
    final case class Number(value: BigInt)       extends Expr
    final case class Bool(value: Boolean)        extends Expr
    final case class StringLit(value: String)    extends Expr
    final case class Symbol(name: String)        extends Expr
    final case class ListExpr(items: List[Expr]) extends Expr

  sealed trait Value
  sealed trait Procedure extends Value

  object Value:
    final case class Number(value: BigInt)                                     extends Value
    final case class Bool(value: Boolean)                                      extends Value
    final case class StringLit(value: String)                                  extends Value
    final case class Symbol(name: String)                                      extends Value
    final case class ListValue(items: List[Value])                             extends Value
    final case class Builtin(name: String, impl: List[Value] => Value)         extends Procedure
    final case class Closure(params: List[String], body: List[Expr], env: Env) extends Procedure
    case object Void                                                           extends Value

  def evalProgram(input: String): Value =
    val expressions = SchemeReader.readAll(input)
    if expressions.isEmpty then throw new EvalError("empty input")
    val env = initialEnv()
    evalSequence(expressions, env)

  def evalProgramWithOutput(input: String): (Value, String) =
    (evalProgram(input), "")

  def render(value: Value): String =
    value match
      case Value.Number(value)    => value.toString
      case Value.Bool(true)       => "#t"
      case Value.Bool(false)      => "#f"
      case Value.StringLit(value) => "\"" + escapeString(value) + "\""
      case Value.Symbol(name)     => name
      case Value.ListValue(items) => items.map(render).mkString("(", " ", ")")
      case _: Procedure           => "#<procedure>"
      case Value.Void             => "#<void>"

  private def evalSequence(expressions: List[Expr], env: Env): Value =
    expressions.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr, env)
    }

  private def eval(expr: Expr, env: Env): Value =
    expr match
      case Expr.Number(value)    => Value.Number(value)
      case Expr.Bool(value)      => Value.Bool(value)
      case Expr.StringLit(value) => Value.StringLit(value)
      case Expr.Symbol(name)     => env.lookup(name)
      case Expr.ListExpr(items) =>
        items match
          case Nil                           => throw new EvalError("cannot evaluate empty list")
          case Expr.Symbol("define") :: args => evalDefine(args, env)
          case Expr.Symbol("begin") :: args  => evalBegin(args, env)
          case Expr.Symbol("if") :: args     => evalIf(args, env)
          case Expr.Symbol("let") :: args    => evalLet(args, env)
          case Expr.Symbol("cond") :: args   => evalCond(args, env)
          case Expr.Symbol("quote") :: args  => evalQuote(args)
          case Expr.Symbol("lambda") :: args => evalLambda(args, env)
          case Expr.Symbol("and") :: args    => evalAnd(args, env)
          case Expr.Symbol("or") :: args     => evalOr(args, env)
          case head :: args =>
            val procedure = eval(head, env)
            val values    = args.map(eval(_, env))
            applyProcedure(procedure, values)

  private def evalDefine(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.Void
      case Expr.ListExpr(Expr.Symbol(name) :: params) :: body if body.nonEmpty =>
        env.define(name, Value.Closure(readParams(params), body, env))
        Value.Void
      case _ =>
        throw new EvalError("invalid define")

  private def evalBegin(args: List[Expr], env: Env): Value =
    evalSequence(args, env)

  private def evalIf(args: List[Expr], env: Env): Value =
    args match
      case condition :: ifTrue :: ifFalse :: Nil =>
        if isTruthy(eval(condition, env)) then eval(ifTrue, env)
        else eval(ifFalse, env)
      case _ =>
        throw new EvalError(s"if expected 3 arguments, got ${args.length}")

  private def evalLet(args: List[Expr], env: Env): Value =
    args match
      case Expr.ListExpr(bindingsExpr) :: body if body.nonEmpty =>
        val bindings = readBindings(bindingsExpr)
        val values   = bindings.map { case (_, valueExpr) => eval(valueExpr, env) }
        val letEnv   = Env.child(env, bindings.map(_._1).zip(values))
        evalSequence(body, letEnv)
      case Expr.Symbol(name) :: Expr.ListExpr(bindingsExpr) :: body if body.nonEmpty =>
        val bindings = readBindings(bindingsExpr)
        val values   = bindings.map { case (_, valueExpr) => eval(valueExpr, env) }
        val letEnv   = Env.child(env, Nil)
        val closure  = Value.Closure(bindings.map(_._1), body, letEnv)
        letEnv.define(name, closure)
        applyProcedure(closure, values)
      case _ =>
        throw new EvalError("invalid let")

  private def evalCond(args: List[Expr], env: Env): Value =
    args match
      case Nil => Value.Void
      case Expr.ListExpr(Expr.Symbol("else") :: body) :: rest =>
        if rest.nonEmpty then throw new EvalError("cond else clause must be last")
        if body.isEmpty then throw new EvalError("cond else clause requires a body")
        evalSequence(body, env)
      case Expr.ListExpr(test :: body) :: rest =>
        val testValue = eval(test, env)
        if isTruthy(testValue) then if body.isEmpty then testValue else evalSequence(body, env)
        else evalCond(rest, env)
      case _ =>
        throw new EvalError("invalid cond clause")

  private def evalQuote(args: List[Expr]): Value =
    args match
      case value :: Nil => quote(value)
      case _            => throw new EvalError(s"quote expected 1 argument, got ${args.length}")

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case Expr.ListExpr(params) :: body if body.nonEmpty =>
        Value.Closure(readParams(params), body, env)
      case _ =>
        throw new EvalError("invalid lambda")

  private def evalAnd(args: List[Expr], env: Env): Value =
    args match
      case Nil => Value.Bool(true)
      case head :: tail =>
        val value = eval(head, env)
        if !isTruthy(value) || tail.isEmpty then value
        else evalAnd(tail, env)

  private def evalOr(args: List[Expr], env: Env): Value =
    args match
      case Nil => Value.Bool(false)
      case head :: tail =>
        val value = eval(head, env)
        if isTruthy(value) || tail.isEmpty then value
        else evalOr(tail, env)

  private def applyProcedure(value: Value, args: List[Value]): Value =
    value match
      case Value.Builtin(_, impl) =>
        impl(args)
      case Value.Closure(params, body, closureEnv) =>
        if args.length != params.length then
          throw new EvalError(s"lambda expected ${params.length} arguments, got ${args.length}")
        val callEnv = Env.child(closureEnv, params.zip(args))
        evalSequence(body, callEnv)
      case other =>
        throw new EvalError(s"not a procedure: ${render(other)}")

  private def initialEnv(): Env =
    val env = Env.root()
    SchemeBuiltins.all.foreach { builtin =>
      env.define(builtin.name, builtin)
    }
    env

  private def readParams(params: List[Expr]): List[String] =
    params.map {
      case Expr.Symbol(name) => name
      case other             => throw new EvalError(s"invalid parameter: ${renderExpr(other)}")
    }

  private def readBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(List(Expr.Symbol(name), valueExpr)) => (name, valueExpr)
      case other => throw new EvalError(s"invalid binding: ${renderExpr(other)}")
    }

  private def quote(expr: Expr): Value =
    expr match
      case Expr.Number(value)    => Value.Number(value)
      case Expr.Bool(value)      => Value.Bool(value)
      case Expr.StringLit(value) => Value.StringLit(value)
      case Expr.Symbol(name)     => Value.Symbol(name)
      case Expr.ListExpr(items)  => Value.ListValue(items.map(quote))

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  final class Env private (parent: Option[Env]):
    private val bindings = mutable.HashMap.empty[String, Value]

    def define(name: String, value: Value): Unit =
      bindings.update(name, value)

    def lookup(name: String): Value =
      bindings.get(name) match
        case Some(value) => value
        case None =>
          parent match
            case Some(parentEnv) => parentEnv.lookup(name)
            case None            => throw new EvalError(s"unbound variable: $name")

  private object Env:
    def root(): Env = new Env(None)

    def child(parent: Env, bindings: Iterable[(String, Value)]): Env =
      val env = new Env(Some(parent))
      bindings.foreach { case (name, value) => env.define(name, value) }
      env

  private def renderExpr(expr: Expr): String =
    expr match
      case Expr.Number(value)    => value.toString
      case Expr.Bool(true)       => "#t"
      case Expr.Bool(false)      => "#f"
      case Expr.StringLit(value) => "\"" + escapeString(value) + "\""
      case Expr.Symbol(name)     => name
      case Expr.ListExpr(items) =>
        items.map(renderExpr).mkString("(", " ", ")")

  private def escapeString(value: String): String =
    val builder = new StringBuilder
    value.foreach {
      case '"'  => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\n' => builder.append("\\n")
      case '\t' => builder.append("\\t")
      case ch   => builder.append(ch)
    }
    builder.result()
