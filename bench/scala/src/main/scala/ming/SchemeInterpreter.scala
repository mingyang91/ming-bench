package ming

import scala.collection.mutable

private[ming] object SchemeInterpreter:

  sealed trait Expr:
    def pos: SourcePos

  object Expr:
    final case class Number(value: BigInt, pos: SourcePos)       extends Expr
    final case class Bool(value: Boolean, pos: SourcePos)        extends Expr
    final case class StringLit(value: String, pos: SourcePos)    extends Expr
    final case class Symbol(name: String, pos: SourcePos)        extends Expr
    final case class ListExpr(items: List[Expr], pos: SourcePos) extends Expr

  sealed trait Value
  sealed trait Procedure extends Value

  object Value:
    final case class Number(value: BigInt)                                          extends Value
    final case class Bool(value: Boolean)                                           extends Value
    final case class StringLit(value: String)                                       extends Value
    final case class Symbol(name: String)                                           extends Value
    final case class ListValue(items: List[Value])                                  extends Value
    final case class Builtin(name: String, impl: (List[Value], SourcePos) => Value) extends Procedure
    final case class Closure(params: List[String], body: List[Expr], env: Env)      extends Procedure
    case object Void                                                                extends Value

  def evalProgram(input: String): Value =
    val expressions = SchemeReader.readAll(input)
    if expressions.isEmpty then throw EvalError.at(SourcePos(1, 1), "empty input")
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
      case Expr.Number(value, _)    => Value.Number(value)
      case Expr.Bool(value, _)      => Value.Bool(value)
      case Expr.StringLit(value, _) => Value.StringLit(value)
      case Expr.Symbol(name, pos)   => env.lookup(name, pos)
      case Expr.ListExpr(items, pos) =>
        items match
          case Nil                              => throw EvalError.at(pos, "cannot evaluate empty list")
          case Expr.Symbol("define", _) :: args => evalDefine(args, env, pos)
          case Expr.Symbol("begin", _) :: args  => evalBegin(args, env)
          case Expr.Symbol("if", _) :: args     => evalIf(args, env, pos)
          case Expr.Symbol("let", _) :: args    => evalLet(args, env, pos)
          case Expr.Symbol("cond", _) :: args   => evalCond(args, env, pos)
          case Expr.Symbol("quote", _) :: args  => evalQuote(args, pos)
          case Expr.Symbol("lambda", _) :: args => evalLambda(args, env, pos)
          case Expr.Symbol("and", _) :: args    => evalAnd(args, env)
          case Expr.Symbol("or", _) :: args     => evalOr(args, env)
          case head :: args =>
            val procedure = eval(head, env)
            val values    = args.map(eval(_, env))
            applyProcedure(procedure, values, pos)

  private def evalDefine(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.Void
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        env.define(name, Value.Closure(readParams(params), body, env))
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid define")

  private def evalBegin(args: List[Expr], env: Env): Value =
    evalSequence(args, env)

  private def evalIf(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case condition :: ifTrue :: ifFalse :: Nil =>
        if isTruthy(eval(condition, env)) then eval(ifTrue, env)
        else eval(ifFalse, env)
      case _ =>
        throw EvalError.at(pos, s"if expected 3 arguments, got ${args.length}")

  private def evalLet(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings = readBindings(bindingsExpr)
        val values   = bindings.map { case (_, valueExpr) => eval(valueExpr, env) }
        val letEnv   = Env.child(env, bindings.map(_._1).zip(values))
        evalSequence(body, letEnv)
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings = readBindings(bindingsExpr)
        val values   = bindings.map { case (_, valueExpr) => eval(valueExpr, env) }
        val letEnv   = Env.child(env, Nil)
        val closure  = Value.Closure(bindings.map(_._1), body, letEnv)
        letEnv.define(name, closure)
        applyProcedure(closure, values, pos)
      case _ =>
        throw EvalError.at(pos, "invalid let")

  private def evalCond(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Nil => Value.Void
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "cond else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "cond else clause requires a body")
        evalSequence(body, env)
      case Expr.ListExpr(test :: body, _) :: rest =>
        val testValue = eval(test, env)
        if isTruthy(testValue) then if body.isEmpty then testValue else evalSequence(body, env)
        else evalCond(rest, env, pos)
      case _ =>
        throw EvalError.at(pos, "invalid cond clause")

  private def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case value :: Nil => quote(value)
      case _            => throw EvalError.at(pos, s"quote expected 1 argument, got ${args.length}")

  private def evalLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.ListExpr(params, _) :: body if body.nonEmpty =>
        Value.Closure(readParams(params), body, env)
      case _ =>
        throw EvalError.at(pos, "invalid lambda")

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

  private def applyProcedure(value: Value, args: List[Value], pos: SourcePos): Value =
    value match
      case Value.Builtin(_, impl) =>
        impl(args, pos)
      case Value.Closure(params, body, closureEnv) =>
        if args.length != params.length then
          throw EvalError.at(pos, s"lambda expected ${params.length} arguments, got ${args.length}")
        val callEnv = Env.child(closureEnv, params.zip(args))
        evalSequence(body, callEnv)
      case other =>
        throw EvalError.at(pos, s"not a procedure: ${render(other)}")

  private def initialEnv(): Env =
    val env = Env.root()
    SchemeBuiltins.all.foreach { builtin =>
      env.define(builtin.name, builtin)
    }
    env

  private def readParams(params: List[Expr]): List[String] =
    params.map {
      case Expr.Symbol(name, _) => name
      case other                => throw EvalError.at(other.pos, s"invalid parameter: ${renderExpr(other)}")
    }

  private def readBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(List(Expr.Symbol(name, _), valueExpr), _) => (name, valueExpr)
      case other => throw EvalError.at(other.pos, s"invalid binding: ${renderExpr(other)}")
    }

  private def quote(expr: Expr): Value =
    expr match
      case Expr.Number(value, _)    => Value.Number(value)
      case Expr.Bool(value, _)      => Value.Bool(value)
      case Expr.StringLit(value, _) => Value.StringLit(value)
      case Expr.Symbol(name, _)     => Value.Symbol(name)
      case Expr.ListExpr(items, _)  => Value.ListValue(items.map(quote))

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  final class Env private (parent: Option[Env]):
    private val bindings = mutable.HashMap.empty[String, Value]

    def define(name: String, value: Value): Unit =
      bindings.update(name, value)

    def lookup(name: String, pos: SourcePos): Value =
      bindings.get(name) match
        case Some(value) => value
        case None =>
          parent match
            case Some(parentEnv) => parentEnv.lookup(name, pos)
            case None            => throw EvalError.at(pos, s"unbound variable: $name")

  private object Env:
    def root(): Env = new Env(None)

    def child(parent: Env, bindings: Iterable[(String, Value)]): Env =
      val env = new Env(Some(parent))
      bindings.foreach { case (name, value) => env.define(name, value) }
      env

  private def renderExpr(expr: Expr): String =
    expr match
      case Expr.Number(value, _)    => value.toString
      case Expr.Bool(true, _)       => "#t"
      case Expr.Bool(false, _)      => "#f"
      case Expr.StringLit(value, _) => "\"" + escapeString(value) + "\""
      case Expr.Symbol(name, _)     => name
      case Expr.ListExpr(items, _) =>
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
