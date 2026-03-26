package ming

import SchemeEvaluatorState.*
import SchemeModel.*
import SchemeRecords.*

private[ming] trait SchemeEvaluatorApplicationSupport extends SchemeEvaluatorSpecialForms:

  protected def evalGuard(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  protected def evalLet(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    callPos: SourcePos
  ): Computation

  protected def evalLetStar(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  protected def evalLetRec(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    sequential: Boolean,
    pos: SourcePos
  ): Computation

  protected def evalDo(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  final protected def evalCompoundExpression(
    list: Expr.ListExpr,
    operator: Expr,
    args: List[Expr],
    env: Env,
    continuation: Continuation
  ): Computation =
    operator match
      case Expr.Symbol(name, _) =>
        env.lookupSyntax(name) match
          case Some(transformer) =>
            val expanded = transformer.expand(list, env)
            eval(expanded.expr, expanded.env, continuation)
          case None =>
            evalApplication(list.pos, operator, args, env, continuation)
      case _ =>
        evalApplication(list.pos, operator, args, env, continuation)

  private def evalApplication(
    callPos: SourcePos,
    operator: Expr,
    args: List[Expr],
    env: Env,
    continuation: Continuation
  ): Computation =
    operator match
      case Expr.Symbol("quote", _) =>
        evalQuote(args, continuation, callPos)
      case Expr.Symbol("syntax", _) =>
        evalSyntax(args, env, continuation, callPos)
      case Expr.Symbol("syntax-case", _) =>
        evalSyntaxCase(args, env, continuation, callPos)
      case Expr.Symbol("if", _) =>
        evalIf(args, env, continuation, callPos)
      case Expr.Symbol("case", _) =>
        evalCase(args, env, continuation, callPos)
      case Expr.Symbol("define", _) =>
        evalDefine(args, env, continuation, callPos)
      case Expr.Symbol("define-syntax", _) =>
        evalDefineSyntax(args, env, continuation, callPos)
      case Expr.Symbol("with-syntax", _) =>
        evalWithSyntax(args, env, continuation, callPos)
      case Expr.Symbol("define-record-type", _) =>
        resume(continuation, defineRecordType(args, env))
      case Expr.Symbol("lambda", _) =>
        evalLambda(args, env, continuation, callPos)
      case Expr.Symbol("case-lambda", _) =>
        evalCaseLambda(args, env, continuation, callPos)
      case Expr.Symbol("set!", _) =>
        evalSet(args, env, continuation, callPos)
      case Expr.Symbol("and", _) =>
        evalAnd(args, env, continuation, callPos)
      case Expr.Symbol("or", _) =>
        evalOr(args, env, continuation, callPos)
      case Expr.Symbol("begin", _) =>
        evalSequence(args, env, continuation)
      case Expr.Symbol("cond", _) =>
        evalCond(args, env, continuation, callPos)
      case Expr.Symbol("guard", _) =>
        evalGuard(args, env, continuation, callPos)
      case Expr.Symbol("let", _) =>
        evalLet(args, env, continuation, callPos)
      case Expr.Symbol("let*", _) =>
        evalLetStar(args, env, continuation, callPos)
      case Expr.Symbol("letrec", _) =>
        evalLetRec(args, env, continuation, sequential = false, callPos)
      case Expr.Symbol("letrec*", _) =>
        evalLetRec(args, env, continuation, sequential = true, callPos)
      case Expr.Symbol("do", _) =>
        evalDo(args, env, continuation, callPos)
      case _ =>
        eval(
          operator,
          env,
          contextualCont(operator.pos) { procedure =>
            evalExprList(args.reverse, env) { reversedArgs =>
              applyProcedure(procedure, reversedArgs.reverse, continuation, Some(callPos))
            }
          }
        )
