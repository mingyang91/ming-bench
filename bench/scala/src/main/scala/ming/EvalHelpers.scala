package ming

import scala.collection.mutable

private[ming] object EvalHelpers:

  def exprToVal(expr: Expr): SchemeVal =
    expr match
      case IntLit(v, _)         => SchemeInt(v)
      case FloatLit(v, _)       => SchemeFloat(v)
      case RationalLit(n, d, _) => SchemeRational(n, d)
      case BoolLit(v, _)        => SchemeBool(v)
      case StringLit(v, _)      => SchemeString(v)
      case CharLit(v, _)        => SchemeChar(v)
      case Symbol(name, _)      => SchemeSymbol(name)
      case SList(elems, _)      => SchemeListOps.makeList(elems.map(exprToVal))

  def bindArgs(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeVal],
    closureEnv: Env
  ): Env =
    restParam match
      case None =>
        if params.size != args.size then throw new EvalError(s"expected ${params.size} arguments, got ${args.size}")
        new Env(mutable.Map.from(params.zip(args)), Some(closureEnv))
      case Some(rest) =>
        if args.size < params.size then
          throw new EvalError(s"expected at least ${params.size} arguments, got ${args.size}")
        val (required, extra) = args.splitAt(params.size)
        val bindings          = mutable.Map.from(params.zip(required))
        bindings(rest) = SchemeListOps.makeList(extra)
        new Env(bindings, Some(closureEnv))

  def commonWindTail(
    a: List[(SchemeVal, SchemeVal)],
    b: List[(SchemeVal, SchemeVal)]
  ): List[(SchemeVal, SchemeVal)] =
    var aa = a; var bb = b
    if aa.length > bb.length then aa = aa.drop(aa.length - bb.length)
    else bb = bb.drop(bb.length - aa.length)
    while !(aa eq bb) do
      aa = aa.tail; bb = bb.tail
    aa
