package ming

import Value.*
import Expr.*

/** Pure (non-CPS) evaluation helpers extracted from Evaluator. */
private[ming] object EvalHelpers:

  def evalError(msg: String, pos: Option[Pos]): Nothing =
    pos match
      case Some(p) => throw new EvalError(s"$msg [$p]")
      case None    => throw new EvalError(msg)

  def evalQuote(args: List[Expr], pos: Option[Pos]): Value =
    args match
      case expr :: Nil => exprToValue(expr)
      case _           => evalError("quote: expected 1 argument", pos)

  def exprToValue(expr: Expr): Value = expr match
    case Num(n, _)    => IntVal(n)
    case Rat(n, d, _) => Value.makeRational(n, d)
    case Flt(d, _)    => FloatVal(d)
    case Bool(b, _)   => BoolVal(b)
    case Str(s, _)    => StrVal(s.toCharArray)
    case Chr(c, _)    => CharVal(c)
    case Sym(s, _)    => SymbolVal(s)
    case SList(elems, _) =>
      elems.foldRight(NilVal: Value)((e, acc) => Pair(exprToValue(e), acc))
    case DottedList(heads, tail, _) =>
      heads.foldRight(exprToValue(tail))((e, acc) => Pair(exprToValue(e), acc))

  def evalLambda(args: List[Expr], env: Env, pos: Option[Pos]): Value =
    args match
      case SList(params, _) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params, "lambda", pos)
        LambdaVal(paramNames, restParam, body, env)
      case DottedList(params, Sym(restName, _), _) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case Sym(p, _) => p
          case _         => evalError("lambda: non-symbol parameter", pos)
        }
        LambdaVal(paramNames, Some(restName), body, env)
      case Sym(restName, _) :: body if body.nonEmpty =>
        LambdaVal(Nil, Some(restName), body, env)
      case _ => evalError("lambda: bad syntax", pos)

  def parseParams(
    params: List[Expr],
    context: String,
    pos: Option[Pos]
  ): (List[String], Option[String]) =
    val dotIdx = params.indexWhere { case Sym(".", _) => true; case _ => false }
    if dotIdx < 0 then
      val names = params.map {
        case Sym(p, _) => p
        case _         => evalError(s"$context: non-symbol parameter", pos)
      }
      (names, None)
    else
      if dotIdx != params.length - 2 then evalError(s"$context: bad dot syntax in parameters", pos)
      val required = params.take(dotIdx).map {
        case Sym(p, _) => p
        case _         => evalError(s"$context: non-symbol parameter", pos)
      }
      val rest = params.last match
        case Sym(p, _) => p
        case _         => evalError(s"$context: non-symbol rest parameter", pos)
      (required, Some(rest))

  def valueToList(v: Value): List[Value] =
    v match
      case NilVal        => Nil
      case PairVal(cell) => cell.car :: valueToList(cell.cdr)
      case _             => throw new EvalError("apply: last argument must be a list")
