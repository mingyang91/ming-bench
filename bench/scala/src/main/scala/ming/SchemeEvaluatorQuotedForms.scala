package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeEvaluatorSupport.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] trait SchemeEvaluatorQuotedForms:

  private enum QuasiquotePart:
    case Item(value: Value)
    case Splice(values: List[Value])

  protected def eval(expr: Expr, env: Env, continuation: Continuation): Computation

  protected def contextualCont(pos: SourcePos)(next: Value => Computation): Continuation

  final protected def evalQuote(
    args: List[Expr],
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case expr :: Nil =>
        resume(continuation, quoteExpr(expr))
      case _ =>
        throw new EvalError(s"quote expected 1 argument, got ${args.length}")

  final protected def evalQuasiquote(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case template :: Nil =>
        evalQuasiquotedExpr(template, env, depth = 1, continuation)
      case _ =>
        throw new EvalError(s"quasiquote expected 1 argument, got ${args.length}")

  private def evalQuasiquotedExpr(
    expr: Expr,
    env: Env,
    depth: Int,
    continuation: Continuation
  ): Computation =
    expr match
      case Expr.ListExpr(List(Expr.Symbol("unquote", _), innerExpr), _) =>
        if depth == 1 then eval(innerExpr, env, continuation)
        else
          evalQuasiquotedExpr(
            innerExpr,
            env,
            depth - 1,
            value => resume(continuation, makeList(List(Value.SymbolValue("unquote"), value)))
          )
      case Expr.ListExpr(List(Expr.Symbol("unquote-splicing", _), innerExpr), _) =>
        if depth == 1 then throw new EvalError("unquote-splicing not in list or vector context")
        else
          evalQuasiquotedExpr(
            innerExpr,
            env,
            depth - 1,
            value =>
              resume(
                continuation,
                makeList(List(Value.SymbolValue("unquote-splicing"), value))
              )
          )
      case Expr.ListExpr(List(Expr.Symbol("quasiquote", _), innerExpr), _) =>
        evalQuasiquotedExpr(
          innerExpr,
          env,
          depth + 1,
          value => resume(continuation, makeList(List(Value.SymbolValue("quasiquote"), value)))
        )
      case Expr.ListExpr(items, _) =>
        evalQuasiquotedList(items, env, depth, continuation)
      case Expr.VectorExpr(items, _) =>
        evalQuasiquotedVector(items, env, depth, continuation)
      case other =>
        resume(continuation, quoteExpr(other))

  private def evalQuasiquotedList(
    items: List[Expr],
    env: Env,
    depth: Int,
    continuation: Continuation
  ): Computation =
    val dotIndices = items.zipWithIndex.collect { case (Expr.Symbol(".", _), index) => index }

    dotIndices match
      case Nil =>
        evalQuasiquotedItems(
          items,
          env,
          depth,
          parts => resume(continuation, buildQuasiquotedList(parts, Value.NilValue))
        )
      case index :: Nil if index > 0 && index == items.length - 2 =>
        evalQuasiquotedItems(
          items.take(index),
          env,
          depth,
          parts =>
            evalQuasiquotedExpr(
              items.last,
              env,
              depth,
              tail => resume(continuation, buildQuasiquotedList(parts, tail))
            )
        )
      case _ =>
        throw new EvalError("invalid dotted list")

  private def evalQuasiquotedVector(
    items: List[Expr],
    env: Env,
    depth: Int,
    continuation: Continuation
  ): Computation =
    evalQuasiquotedItems(
      items,
      env,
      depth,
      parts =>
        resume(
          continuation,
          Value.VectorValue(scala.collection.mutable.ArrayBuffer.from(flattenQuasiquoteParts(parts)))
        )
    )

  private def evalQuasiquotedItems(
    items: List[Expr],
    env: Env,
    depth: Int,
    continuation: List[QuasiquotePart] => Computation
  ): Computation =
    def loop(
      remaining: List[Expr],
      reversedParts: List[QuasiquotePart]
    ): Computation =
      remaining match
        case Nil =>
          suspend(continuation(reversedParts.reverse))
        case item :: tail =>
          evalQuasiquotedItem(
            item,
            env,
            depth,
            part => suspend(loop(tail, part :: reversedParts))
          )

    loop(items, Nil)

  private def evalQuasiquotedItem(
    expr: Expr,
    env: Env,
    depth: Int,
    continuation: QuasiquotePart => Computation
  ): Computation =
    expr match
      case Expr.ListExpr(List(Expr.Symbol("unquote-splicing", _), innerExpr), _) if depth == 1 =>
        eval(
          innerExpr,
          env,
          contextualCont(innerExpr.pos) { value =>
            suspend(
              continuation(
                QuasiquotePart.Splice(properListElements("unquote-splicing", value))
              )
            )
          }
        )
      case _ =>
        evalQuasiquotedExpr(
          expr,
          env,
          depth,
          value => suspend(continuation(QuasiquotePart.Item(value)))
        )

  private def buildQuasiquotedList(parts: List[QuasiquotePart], tail: Value): Value =
    flattenQuasiquoteParts(parts).foldRight(tail) { (value, cdr) =>
      Value.PairValue(value, cdr)
    }

  private def flattenQuasiquoteParts(parts: List[QuasiquotePart]): List[Value] =
    parts.flatMap {
      case QuasiquotePart.Item(value)    => List(value)
      case QuasiquotePart.Splice(values) => values
    }
