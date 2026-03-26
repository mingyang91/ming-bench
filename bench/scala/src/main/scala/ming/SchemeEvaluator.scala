package ming

import SchemeModel.*
import SchemeRecords.*

private[ming] object SchemeEvaluator extends SchemeEvaluatorSpecialForms:

  override def evalSequence(expressions: List[Expr], env: Env): Value =
    expressions.foldLeft(Value.VoidValue: Value) { (_, expr) =>
      eval(expr, env)
    }

  def applyProcedure(procedure: Value, args: List[Value]): Value =
    procedure match
      case Value.Builtin(_, implementation) => implementation(args)
      case caseClosure: Value.CaseClosure   => applyCaseClosure(caseClosure, args)
      case closure: Value.Closure           => applyClosure(closure, args)
      case other =>
        throw new EvalError(s"not a procedure: ${SchemeRuntime.render(other)}")

  override protected def eval(expr: Expr, env: Env): Value =
    withErrorContext(expr.pos) {
      expr match
        case Expr.IntegerLiteral(value, _) => Value.IntegerValue(value)
        case Expr.RationalLiteral(numerator, denominator, _) =>
          SchemeNumbers.exactRational(numerator, denominator)
        case Expr.InexactLiteral(value, _) => Value.InexactValue(value)
        case Expr.BooleanLiteral(value, _) => Value.BooleanValue(value)
        case Expr.StringLiteral(value, _)  => Value.StringValue(SchemeString.fromText(value))
        case Expr.CharLiteral(value, _)    => Value.CharValue(value)
        case Expr.Symbol(name, _)          => env.lookup(name)
        case Expr.ListExpr(Nil, _) =>
          throw new EvalError("cannot evaluate an empty list")
        case list @ Expr.ListExpr(operator :: args, _) =>
          evalCompoundExpression(list, operator, args, env)
    }

  private def evalCompoundExpression(
    list: Expr.ListExpr,
    operator: Expr,
    args: List[Expr],
    env: Env
  ): Value =
    operator match
      case Expr.Symbol(name, _) =>
        env.lookupSyntax(name) match
          case Some(transformer) =>
            val expanded = transformer.expand(list, env)
            eval(expanded.expr, expanded.env)
          case None =>
            evalApplication(operator, args, env)
      case _ =>
        evalApplication(operator, args, env)

  private def evalApplication(operator: Expr, args: List[Expr], env: Env): Value =
    operator match
      case Expr.Symbol("quote", _) =>
        evalQuote(args)
      case Expr.Symbol("if", _) =>
        evalIf(args, env)
      case Expr.Symbol("case", _) =>
        evalCase(args, env)
      case Expr.Symbol("define", _) =>
        evalDefine(args, env)
      case Expr.Symbol("define-syntax", _) =>
        evalDefineSyntax(args, env)
      case Expr.Symbol("define-record-type", _) =>
        defineRecordType(args, env)
      case Expr.Symbol("lambda", _) =>
        evalLambda(args, env)
      case Expr.Symbol("case-lambda", _) =>
        evalCaseLambda(args, env)
      case Expr.Symbol("set!", _) =>
        evalSet(args, env)
      case Expr.Symbol("and", _) =>
        evalAnd(args, env)
      case Expr.Symbol("or", _) =>
        evalOr(args, env)
      case Expr.Symbol("begin", _) =>
        evalBegin(args, env)
      case Expr.Symbol("cond", _) =>
        evalCond(args, env)
      case Expr.Symbol("let", _) =>
        evalLet(args, env)
      case Expr.Symbol("letrec", _) =>
        evalLetRec(args, env, sequential = false)
      case Expr.Symbol("letrec*", _) =>
        evalLetRec(args, env, sequential = true)
      case Expr.Symbol("do", _) =>
        evalDo(args, env)
      case _ =>
        applyProcedure(eval(operator, env), args.map(arg => eval(arg, env)))

  private def withErrorContext[T](pos: SourcePos)(thunk: => T): T =
    try thunk
    catch
      case error: EvalError if error.position.isEmpty =>
        throw error.withPosition(pos)
