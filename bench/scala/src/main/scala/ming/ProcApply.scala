package ming

import SchemeValue.*
import Evaluator.{EvalS, ReturnS, Step}

/** Procedure application: lambda calls, apply, map, for-each. */
private[ming] object ProcApply:

  def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step = proc match
    case SchemeLambda(params, restParam, body, closure) =>
      val localEnv = bindArgs(params, restParam, args, closure)
      SpecialForms.startSequence(body, localEnv, k, out)
    case SchemeContinuation(savedK) =>
      if args.length != 1 then throw new EvalError("continuation: expected 1 argument")
      ReturnS(args.head, savedK, out)
    case SchemeBuiltinProc("call/cc") | SchemeBuiltinProc("call-with-current-continuation") =>
      if args.length != 1 then throw new EvalError("call/cc: expected 1 argument")
      val kontVal = SchemeContinuation(k)
      applyProc(args.head, List(kontVal), k, out)
    case SchemeBuiltinProc("apply") =>
      applyApply(args, k, out)
    case SchemeBuiltinProc("map") =>
      applyMap(args, k, out)
    case SchemeBuiltinProc("for-each") =>
      applyForEach(args, k, out)
    case SchemeBuiltinProc(name) =>
      val (result, bo) = Builtins.evalBuiltin(name, args)
      ReturnS(result, k, out + bo)
    case other =>
      throw new EvalError(s"not a procedure: ${other.display}")

  private def bindArgs(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    closure: Env
  ): Env = restParam match
    case None =>
      if params.length != args.length then
        throw new EvalError(
          s"expected ${params.length} arguments, got ${args.length}"
        )
      closure.extend(params, args)
    case Some(rest) =>
      if args.length < params.length then
        throw new EvalError(
          s"expected at least ${params.length} arguments, got ${args.length}"
        )
      val (fixed, remaining) = args.splitAt(params.length)
      closure.extend(params :+ rest, fixed :+ SchemeList(remaining))

  private def applyApply(
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step =
    if args.length < 2 then throw new EvalError("apply: expected at least 2 arguments")
    val proc       = args.head
    val lastArg    = args.last
    val prefixArgs = args.drop(1).dropRight(1)
    val allArgs    = prefixArgs ++ Builtins.asList(lastArg)
    applyProc(proc, allArgs, k, out)

  private def applyMap(
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step =
    if args.length < 2 then throw new EvalError("map: expected at least 2 arguments")
    val proc  = args.head
    val lists = args.tail.map(Builtins.asList)
    if lists.isEmpty then ReturnS(SchemeList(Nil), k, out)
    else
      val len = lists.head.length
      if !lists.tail.forall(_.length == len) then throw new EvalError("map: lists must have same length")
      if len == 0 then ReturnS(SchemeList(Nil), k, out)
      else
        val groups = (0 until len).toList.map(i => lists.map(_(i)))
        groups match
          case first :: rest =>
            applyProc(proc, first, Kont.MapK(proc, rest, Nil, k), out)
          case Nil => ReturnS(SchemeList(Nil), k, out)

  private def applyForEach(
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step =
    if args.length < 2 then throw new EvalError("for-each: expected at least 2 arguments")
    val proc  = args.head
    val lists = args.tail.map(Builtins.asList)
    if lists.isEmpty then ReturnS(SchemeVoid, k, out)
    else
      val len = lists.head.length
      if !lists.tail.forall(_.length == len) then throw new EvalError("for-each: lists must have same length")
      if len == 0 then ReturnS(SchemeVoid, k, out)
      else
        val groups = (0 until len).toList.map(i => lists.map(_(i)))
        groups match
          case first :: rest =>
            applyProc(proc, first, Kont.ForEachK(proc, rest, k), out)
          case Nil => ReturnS(SchemeVoid, k, out)
