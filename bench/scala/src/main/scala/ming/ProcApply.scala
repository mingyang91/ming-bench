package ming

import SchemeValue.*
import Evaluator.{EvalS, RaiseS, ReturnS, Step}

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
    case np: SchemeNativeProc =>
      ReturnS(np.fn(args), k, out)
    case SchemeContinuation(savedK) =>
      if args.length != 1 then throw new EvalError("continuation: expected 1 argument")
      windTransition(k, savedK, args.head, out)
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
    case SchemeBuiltinProc("dynamic-wind") =>
      applyDynamicWind(args, k, out)
    case SchemeBuiltinProc("raise") =>
      if args.length != 1 then throw new EvalError("raise: expected 1 argument")
      RaiseS(args.head, k, out)
    case SchemeBuiltinProc("with-exception-handler") =>
      if args.length != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
      applyProc(args(1), Nil, Kont.ExceptionHandlerK(args(0), k), out)
    case SchemeBuiltinProc("values") =>
      args match
        case single :: Nil => ReturnS(single, k, out)
        case _             => ReturnS(SchemeMultipleValues(args), k, out)
    case SchemeBuiltinProc("call-with-values") =>
      if args.length != 2 then throw new EvalError("call-with-values: expected 2 arguments")
      applyProc(args(0), Nil, Kont.CallWithValuesK(args(1), k), out)
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
      closure.extend(params :+ rest, fixed :+ Builtins.toPairChain(remaining))

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

  private def applyDynamicWind(
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step =
    if args.length != 3 then throw new EvalError("dynamic-wind: expected 3 arguments")
    val inThunk   = args(0)
    val bodyThunk = args(1)
    val outThunk  = args(2)
    val entry     = new WinderEntry(inThunk, outThunk)
    applyProc(inThunk, Nil, Kont.DynWindInK(bodyThunk, entry, k), out)

  private[ming] def windTransition(
    currentK: Kont,
    targetK: Kont,
    value: SchemeValue,
    out: String
  ): Step =
    val currentWinders       = extractWinders(currentK)
    val targetWinders        = extractWinders(targetK)
    val (toUnwind, toRewind) = computeWindDiff(currentWinders, targetWinders)
    if toUnwind.isEmpty && toRewind.isEmpty then ReturnS(value, targetK, out)
    else startUnwind(toUnwind, toRewind, value, targetK, out)

  private[ming] def startUnwind(
    toUnwind: List[WinderEntry],
    toRewind: List[WinderEntry],
    value: SchemeValue,
    targetK: Kont,
    out: String
  ): Step = toUnwind match
    case Nil => startRewind(toRewind, value, targetK, out)
    case entry :: rest =>
      applyProc(entry.outThunk, Nil, Kont.DynUnwindK(rest, toRewind, value, targetK), out)

  private[ming] def startRewind(
    toRewind: List[WinderEntry],
    value: SchemeValue,
    targetK: Kont,
    out: String
  ): Step = toRewind match
    case Nil => ReturnS(value, targetK, out)
    case entry :: rest =>
      applyProc(entry.inThunk, Nil, Kont.DynRewindK(rest, value, targetK), out)

  private def extractWinders(k: Kont): List[WinderEntry] =
    @scala.annotation.tailrec
    def loop(k: Kont, acc: List[WinderEntry]): List[WinderEntry] = k match
      case Kont.DynWindMark(entry, next) => loop(next, entry :: acc)
      case Kont.Halt                     => acc.reverse
      case other                         => loop(parentKont(other), acc)
    loop(k, Nil)

  private def parentKont(k: Kont): Kont = k match
    case Kont.Halt                                  => Kont.Halt
    case Kont.Seq(_, _, next)                       => next
    case Kont.EvalOp(_, _, next, _)                 => next
    case Kont.EvalArg(_, _, _, _, next, _)          => next
    case Kont.IfK(_, _, _, next)                    => next
    case Kont.SetK(_, _, next)                      => next
    case Kont.DefineK(_, _, next)                   => next
    case Kont.AndK(_, _, next)                      => next
    case Kont.OrK(_, _, next)                       => next
    case Kont.CallCCK(next)                         => next
    case Kont.CondK(_, _, _, next)                  => next
    case Kont.LetInitK(_, _, _, _, _, next)         => next
    case Kont.NamedLetInitK(_, _, _, _, _, _, next) => next
    case Kont.MapK(_, _, _, next)                   => next
    case Kont.LetrecInitK(_, _, _, _, _, next)      => next
    case Kont.LetStarInitK(_, _, _, _, next)        => next
    case Kont.CaseK(_, _, next)                     => next
    case Kont.ForEachK(_, _, next)                  => next
    case Kont.DynWindMark(_, next)                  => next
    case Kont.DynWindInK(_, _, next)                => next
    case Kont.DynWindRetK(_, next)                  => next
    case Kont.DynUnwindK(_, _, _, next)             => next
    case Kont.DynRewindK(_, _, next)                => next
    case Kont.ExceptionHandlerK(_, next)            => next
    case Kont.GuardK(_, _, _, next)                 => next
    case Kont.GuardTestK(_, _, _, _, _, _, next)    => next
    case Kont.GuardClauseK(_, _, next)              => next
    case Kont.CallWithValuesK(_, next)              => next
    case Kont.SyntaxCaseK(_, _, _, next)            => next
    case Kont.WithSyntaxK(_, _, _, _, next)         => next

  private def computeWindDiff(
    current: List[WinderEntry],
    target: List[WinderEntry]
  ): (List[WinderEntry], List[WinderEntry]) =
    val lc = current.length
    val lt = target.length
    val (cc, tt) =
      if lc > lt then (current.drop(lc - lt), target)
      else (current, target.drop(lt - lc))

    @scala.annotation.tailrec
    def findCommonLen(a: List[WinderEntry], b: List[WinderEntry]): Int =
      (a, b) match
        case (ah :: at, bh :: bt) =>
          if ah eq bh then a.length else findCommonLen(at, bt)
        case _ => 0

    val commonLen = findCommonLen(cc, tt)
    val toUnwind  = current.take(lc - commonLen)
    val toRewind  = target.take(lt - commonLen).reverse
    (toUnwind, toRewind)
