package ming

import SchemeValue.*

/** Core eval logic with trampoline-based TCO. */
object Interpreter:

  /** Result of a special form: either a final value or a tail-call continuation. */
  private[ming] enum EvalResult:
    case Done(value: SchemeValue)
    case TailCall(expr: SchemeValue, env: Environment)

  import EvalResult.*

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  /** Evaluate all but the last expression in a body, returning the last unevaluated. */
  private[ming] def evalBodyInit(body: List[SchemeValue], env: Environment): Unit =
    var remaining = body
    while remaining.tail.nonEmpty do
      ContinuationManager.contextStack = BodyContext(remaining.tail, env) :: ContinuationManager.contextStack
      ContinuationManager.hasSameBodyFrame = true
      ContinuationManager.bodyContext = BodyContext(List(remaining.head), env)
      eval(remaining.head, env)
      ContinuationManager.hasSameBodyFrame = false
      ContinuationManager.contextStack = ContinuationManager.contextStack.tail
      remaining = remaining.tail
    if remaining.nonEmpty then ContinuationManager.bodyContext = BodyContext(remaining, env)

  def eval(expr0: SchemeValue, env0: Environment): SchemeValue =
    var curExpr: SchemeValue = expr0
    var curEnv: Environment  = env0

    while true do
      curExpr match
        // Self-evaluating
        case IntVal(_, _) | RationalVal(_, _, _) | DoubleVal(_, _) | BoolVal(_, _) | StringVal(_, _) |
            MutableStringVal(_, _) | CharVal(_, _) | PairVal(_, _) | MutablePairVal(_) | VectorVal(_, _) |
            LambdaVal(_, _, _, _) | BuiltinVal(_, _) | ContinuationVal(_, _, _, _, _, _, _) |
            SyntaxRulesVal(_, _, _, _) | SyntaxTransformerVal(_) | ValuesVal(_) | RecordVal(_, _, _) | Void =>
          return curExpr

        // Symbol lookup
        case SymbolVal(name, pos) =>
          return curEnv.get(name).getOrElse(throw new EvalError(posMsg(s"unbound variable: $name", pos)))

        // Empty application
        case ListVal(Nil, pos) =>
          throw new EvalError(posMsg("empty application", pos))

        // Special forms
        case ListVal(SymbolVal("define", _) :: args, pos) =>
          return SpecialForms.evalDefine(args, pos, curEnv)

        case ListVal(SymbolVal("quote", _) :: args, pos) =>
          return SpecialForms.evalQuote(args, pos)

        case ListVal(SymbolVal("lambda", _) :: args, pos) =>
          return SpecialForms.evalLambda(args, pos, curEnv)

        case ListVal(SymbolVal("set!", _) :: args, pos) =>
          return SpecialForms.evalSet(args, pos, curEnv)

        case ListVal(SymbolVal("begin", _) :: args, _) =>
          if args.isEmpty then return Void
          evalBodyInit(args, curEnv)
          curExpr = args.last

        case ListVal(SymbolVal("define-syntax", _) :: args, pos) =>
          return SpecialForms.evalDefineSyntax(args, pos, curEnv)

        case ListVal(SymbolVal("guard", _) :: args, pos) =>
          return WindException.evalGuard(args, pos, curEnv)

        case ListVal(SymbolVal("define-record-type", _) :: args, pos) =>
          return RecordForms.evalDefineRecordType(args, pos, curEnv)

        case ListVal(SymbolVal("syntax-case", _) :: args, pos) =>
          return SyntaxCase.evalSyntaxCase(args, pos, curEnv)

        case ListVal(SymbolVal("syntax-quote", _) :: template :: Nil, _) =>
          return SyntaxCase.evalSyntaxQuote(template)

        case ListVal(SymbolVal("with-syntax", _) :: args, pos) =>
          return SyntaxCase.evalWithSyntax(args, pos, curEnv)

        case ListVal(SymbolVal(name, _) :: args, pos) if tailCallForms.contains(name) =>
          dispatchTailForm(name, args, pos, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

        // Macro expansion
        case ListVal((sym @ SymbolVal(name, _)) :: _, pos) =>
          curEnv.get(name) match
            case Some(SyntaxRulesVal(mn, lits, rules, defEnv)) =>
              curExpr = Macro.expand(mn, lits, rules, defEnv, curExpr)
            case Some(SyntaxTransformerVal(proc)) =>
              curExpr = applyProc(proc, List(curExpr), pos)
            case _ =>
              val args = curExpr.asInstanceOf[ListVal].elements.tail
              evalApplication(sym, args, pos, curEnv) match
                case Done(v)          => return v
                case TailCall(e, env) => curExpr = e; curEnv = env

        // Procedure application (non-symbol head)
        case ListVal(head :: args, pos) =>
          evalApplication(head, args, pos, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

    // Unreachable but needed for type checker
    throw new AssertionError("unreachable")

  private val tailCallForms: Set[String] =
    Set("if", "and", "or", "let", "cond", "letrec", "letrec*", "case", "do", "let*")

  private def dispatchTailForm(
    name: String,
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    name match
      case "if"      => evalIf(args, pos, env)
      case "and"     => evalAnd(args, env)
      case "or"      => evalOr(args, env)
      case "let"     => SpecialForms.evalLet(args, pos, env)
      case "cond"    => SpecialForms.evalCond(args, env)
      case "letrec"  => SpecialForms.evalLetrec(args, pos, env)
      case "letrec*" => SpecialForms.evalLetrecStar(args, pos, env)
      case "case"    => SpecialForms.evalCase(args, pos, env)
      case "do"      => SpecialForms.evalDo(args, pos, env)
      case "let*"    => SpecialForms.evalLetStar(args, pos, env)
      case _         => throw new AssertionError(s"unreachable: $name")

  def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue],
    callPos: Option[SourcePos] = None
  ): SchemeValue =
    proc match
      case LambdaVal(params, restParam, body, closure) =>
        bindAndEvalBody(params, restParam, args, body, closure, callPos)
      case BuiltinVal(_, func) =>
        try func(args)
        catch
          case e: EvalError =>
            if callPos.isDefined && !e.getMessage.matches(".*\\d+:\\d+.*") then
              throw new EvalError(posMsg(e.getMessage, callPos))
            else throw e
      case ContinuationVal(contId, bodyExprs, bodyEnv, ctxStack, seqRem, seqE, hasSame) =>
        if args.length != 1 then throw new EvalError(posMsg("continuation: requires 1 argument", callPos))
        throw new ContinuationJump(contId, args.head, bodyExprs, bodyEnv, ctxStack, seqRem, seqE, hasSame)
      case _ =>
        throw new EvalError(posMsg("not a procedure", callPos))

  /** Bind parameters (including rest param) and evaluate body with TCO on last expr. */
  private def bindParamsAndTailCall(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    body: List[SchemeValue],
    closure: Environment,
    callPos: Option[SourcePos]
  ): EvalResult =
    val childEnv = bindParams(params, restParam, args, closure, callPos)
    evalBodyInit(body, childEnv)
    TailCall(body.last, childEnv)

  private def bindAndEvalBody(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    body: List[SchemeValue],
    closure: Environment,
    callPos: Option[SourcePos]
  ): SchemeValue =
    val savedCtx = ContinuationManager.bodyContext
    val childEnv = bindParams(params, restParam, args, closure, callPos)
    try
      evalBodyInit(body, childEnv)
      eval(body.last, childEnv)
    finally ContinuationManager.bodyContext = savedCtx

  private def bindParams(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    closure: Environment,
    callPos: Option[SourcePos]
  ): Environment =
    restParam match
      case Some(rest) =>
        if args.length < params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected at least ${params.length}, got ${args.length}", callPos)
          )
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        childEnv.define(rest, ListVal(args.drop(params.length)))
        childEnv
      case None =>
        if args.length != params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected ${params.length}, got ${args.length}", callPos)
          )
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        childEnv

  private def evalIf(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): EvalResult =
    args match
      case cond :: thenBr :: elseBr :: Nil =>
        if eval(cond, env).isTruthy then TailCall(thenBr, env)
        else TailCall(elseBr, env)
      case cond :: thenBr :: Nil =>
        if eval(cond, env).isTruthy then TailCall(thenBr, env)
        else Done(Void)
      case _ => throw new EvalError(posMsg("if: bad syntax", pos))

  private def evalAnd(args: List[SchemeValue], env: Environment): EvalResult =
    args match
      case Nil         => Done(BoolVal(true))
      case last :: Nil => TailCall(last, env)
      case _ =>
        var remaining = args
        while remaining.tail.nonEmpty do
          val v = eval(remaining.head, env)
          if !v.isTruthy then return Done(v)
          remaining = remaining.tail
        TailCall(remaining.head, env)

  private def evalOr(args: List[SchemeValue], env: Environment): EvalResult =
    args match
      case Nil         => Done(BoolVal(false))
      case last :: Nil => TailCall(last, env)
      case _ =>
        var remaining = args
        while remaining.tail.nonEmpty do
          val v = eval(remaining.head, env)
          if v.isTruthy then return Done(v)
          remaining = remaining.tail
        TailCall(remaining.head, env)

  private def evalApplication(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    val proc       = eval(head, env)
    val evaledArgs = args.map(eval(_, env))
    proc match
      case LambdaVal(params, restParam, body, closure) =>
        bindParamsAndTailCall(params, restParam, evaledArgs, body, closure, pos)
      case BuiltinVal(_, func) =>
        try Done(func(evaledArgs))
        catch
          case e: EvalError =>
            if pos.isDefined && !e.getMessage.matches(".*\\d+:\\d+.*") then
              throw new EvalError(posMsg(e.getMessage, pos))
            else throw e
      case ContinuationVal(contId, bodyExprs, bodyEnv, ctxStack, seqRem, seqE, hasSame) =>
        if evaledArgs.length != 1 then throw new EvalError(posMsg("continuation: requires 1 argument", pos))
        throw new ContinuationJump(contId, evaledArgs.head, bodyExprs, bodyEnv, ctxStack, seqRem, seqE, hasSame)
      case _ =>
        throw new EvalError(posMsg("not a procedure", pos))
