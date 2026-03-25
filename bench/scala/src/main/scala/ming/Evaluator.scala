package ming

import DynamicWind.Winder

/** Scheme interpreter entry point — CPS-based with trampoline for call/cc support. */
object Evaluator:

  // --- CPS types ---
  sealed trait Bounce
  case class Done(value: SchemeVal)    extends Bounce
  case class More(thunk: () => Bounce) extends Bounce

  type Cont = SchemeVal => Bounce

  /** Thrown when a continuation is invoked from a non-CPS context (e.g., inside a builtin). */
  class ContinuationThrown(val bounce: Bounce) extends Exception with scala.util.control.NoStackTrace

  // --- dynamic-wind support ---
  var winders: List[Winder] = Nil

  // --- exception handling support ---
  type ExceptionHandler = SchemeVal => Bounce
  var exceptionHandlers: List[ExceptionHandler] = Nil

  // --- Entry points ---
  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    val env    = Env.default()
    val result = evalProgram(exprs, env)
    SchemeVal.display(result)

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    val output = new StringBuilder
    val env    = Env.defaultWithOutput(output)
    val result = evalProgram(exprs, env)
    (SchemeVal.display(result), output.toString)

  private def evalProgram(exprs: List[SchemeVal], env: Env): SchemeVal =
    winders = Nil
    exceptionHandlers = Nil
    run(evalBodyK(exprs, env, v => Done(v)))

  // --- Trampoline (top-level, catches ContinuationThrown) ---
  private def run(initial: Bounce): SchemeVal =
    var b = initial
    while true do
      b match
        case Done(v) => return v
        case More(thunk) =>
          try b = thunk()
          catch case ct: ContinuationThrown => b = ct.bounce
    throw new AssertionError("unreachable")

  /** Inner trampoline for non-CPS callers; propagates ContinuationThrown. */
  private def runInner(initial: Bounce): SchemeVal =
    var b = initial
    while true do
      b match
        case Done(v)     => return v
        case More(thunk) => b = thunk()
    throw new AssertionError("unreachable")

  // --- Public backward-compat methods ---

  /** Evaluate an expression (non-CPS wrapper). */
  def eval(expr: SchemeVal, env: Env): SchemeVal =
    runInner(evalK(expr, env, v => Done(v)))

  /** Apply a procedure (non-CPS, called from builtins like map/for-each/apply). */
  def apply(proc: SchemeVal, args: List[SchemeVal]): SchemeVal =
    proc match
      case SchemeVal.ContinuationVal(savedState) =>
        val v                      = if args.isEmpty then SchemeVal.Void else args.head
        val (savedK, savedWinders) = savedState.asInstanceOf[(Cont, List[Winder])]
        val currentWinders         = winders
        throw new ContinuationThrown(DynamicWind.doWindK(currentWinders, savedWinders, () => savedK(v)))
      case SchemeVal.CallCCVal() =>
        if args.size != 1 then throw new EvalError("call/cc: expected 1 argument")
        val savedWinders = winders
        val localK: Cont = v => Done(v)
        val kontVal      = SchemeVal.ContinuationVal((localK, savedWinders))
        apply(args.head, List(kontVal))
      case _ =>
        runInner(applyK(proc, args, v => Done(v)))

  def applyProc(proc: SchemeVal, args: List[SchemeVal]): SchemeVal =
    apply(proc, args)

  /** Evaluate body returning TailCall for last expr (backward compat for Apply.scala). */
  def evalBodyTail(body: List[SchemeVal], env: Env): SchemeVal =
    if body.isEmpty then SchemeVal.Void
    else
      body.init.foreach(e => eval(e, env))
      SchemeVal.TailCall(body.last, env)

  def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  /** Check if a value is user-defined (not a builtin). Used to detect shadowing of special forms. */
  private def isUserDefined(v: SchemeVal): Boolean = v match
    case SchemeVal.LambdaProc(_, _, _, _) => true
    case SchemeVal.CaseLambdaProc(_, _)   => true
    case SchemeVal.ContinuationVal(_)     => true
    case _                                => false

  // --- CPS eval ---
  def evalK(expr: SchemeVal, env: Env, k: Cont): Bounce =
    try evalKImpl(expr, env, k)
    catch
      case e: EvalError =>
        val (line, col) = expr.pos
        if line > 0 && !e.getMessage.matches(".*\\d+:\\d+.*") then throw new EvalError(s"$line:$col: ${e.getMessage}")
        else throw e

  private def evalKImpl(expr: SchemeVal, env: Env, k: Cont): Bounce =
    expr match
      case SchemeVal.Symbol(name) =>
        env.lookup(name) match
          case Some(v) => k(v)
          case None    => throw new EvalError(s"unbound variable: $name")

      case SchemeVal.SList(elems) if elems.isEmpty =>
        throw new EvalError("empty application")

      case SchemeVal.SList(elems) =>
        elems.head match
          case SchemeVal.Symbol("define") => SpecialForms.evalDefineK(elems.tail, env, k)
          case SchemeVal.Symbol("if")     => SpecialForms.evalIfK(elems.tail, env, k)
          case SchemeVal.Symbol("quote") =>
            if elems.tail.size != 1 then throw new EvalError("quote: expected 1 argument")
            k(elems.tail.head)
          case SchemeVal.Symbol("lambda")       => k(SpecialForms.evalLambda(elems.tail, env))
          case SchemeVal.Symbol("and")          => SpecialForms.evalAndK(elems.tail, env, k)
          case SchemeVal.Symbol("or")           => SpecialForms.evalOrK(elems.tail, env, k)
          case SchemeVal.Symbol("begin")        => evalBodyK(elems.tail, env, k)
          case SchemeVal.Symbol("dynamic-wind") => DynamicWind.evalDynamicWindK(elems.tail, env, k)
          case SchemeVal.Symbol("raise") if !env.lookup("raise").exists(isUserDefined) =>
            if elems.tail.size != 1 then throw new EvalError("raise: expected 1 argument")
            evalK(
              elems.tail.head,
              env,
              value =>
                if exceptionHandlers.isEmpty then
                  throw new EvalError(s"unhandled exception: ${SchemeVal.display(value)}")
                val handler = exceptionHandlers.head
                exceptionHandlers = exceptionHandlers.tail
                handler(value)
            )
          case SchemeVal.Symbol("with-exception-handler")
              if !env.lookup("with-exception-handler").exists(isUserDefined) =>
            if elems.tail.size != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
            evalK(
              elems.tail(0),
              env,
              handlerProc =>
                evalK(
                  elems.tail(1),
                  env,
                  thunkProc =>
                    val savedHandlers = exceptionHandlers
                    val cpsHandler: ExceptionHandler =
                      value => applyK(handlerProc, List(value), _ => throw new EvalError("raise: handler returned"))
                    exceptionHandlers = cpsHandler :: exceptionHandlers
                    applyK(
                      thunkProc,
                      Nil,
                      result =>
                        exceptionHandlers = savedHandlers
                        k(result)
                    )
                )
            )
          case SchemeVal.Symbol("guard") =>
            Guard.evalGuardK(elems.tail, env, k)
          case SchemeVal.Symbol("call-with-values") if !env.lookup("call-with-values").exists(isUserDefined) =>
            if elems.tail.size != 2 then throw new EvalError("call-with-values: expected 2 arguments")
            evalK(elems.tail(0), env, producer =>
              evalK(elems.tail(1), env, consumer =>
                applyK(producer, Nil, result =>
                  result match
                    case SchemeVal.MultipleValues(vals) => applyK(consumer, vals, k)
                    case other                         => applyK(consumer, List(other), k)
                )
              )
            )
          case SchemeVal.Symbol("let")  => BindingForms.evalLetK(elems.tail, env, k)
          case SchemeVal.Symbol("cond") => BindingForms.evalCondK(elems.tail, env, k)
          case SchemeVal.Symbol("set!") => SpecialForms.evalSetK(elems.tail, env, k)
          case SchemeVal.Symbol("define-syntax") =>
            SpecialForms.evalDefineSyntax(elems.tail, env)
            k(SchemeVal.Void)
          case SchemeVal.Symbol("case-lambda") => k(SpecialForms.evalCaseLambda(elems.tail, env))
          case SchemeVal.Symbol("letrec")      => BindingForms.evalLetrecK(elems.tail, env, k)
          case SchemeVal.Symbol("letrec*")     => BindingForms.evalLetrecStarK(elems.tail, env, k)
          case SchemeVal.Symbol("case")        => BindingForms.evalCaseK(elems.tail, env, k)
          case SchemeVal.Symbol("do")          => BindingForms.evalDoK(elems.tail, env, k)
          case SchemeVal.Symbol("let*")        => BindingForms.evalLetStarK(elems.tail, env, k)
          case SchemeVal.Symbol("define-record-type") =>
            RecordType.evalDefineRecordType(elems.tail, env)
            k(SchemeVal.Void)
          case SchemeVal.Symbol(name) =>
            env.lookup(name) match
              case Some(m: SchemeVal.MacroVal) =>
                val expanded = Macro.expand(m.name, m.literals, m.rules, m.defEnv, SchemeVal.SList(elems))
                More(() => evalK(expanded, env, k))
              case _ => evalApplicationK(elems, expr.pos, env, k)
          case _ => evalApplicationK(elems, expr.pos, env, k)

      case SchemeVal.DottedList(_, _) => throw new EvalError(s"cannot evaluate dotted list: $expr")
      case _                          => k(expr) // self-evaluating

  // --- Application (right-to-left arg evaluation for correct call/cc capture) ---
  private def evalApplicationK(elems: List[SchemeVal], callPos: (Int, Int), env: Env, k: Cont): Bounce =
    evalK(
      elems.head,
      env,
      proc =>
        val argExprs = elems.tail
        var argK: List[SchemeVal] => Bounce = args =>
          More(() =>
            try applyK(proc, args, k)
            catch
              case e: EvalError =>
                val (line, col) = callPos
                if line > 0 && !e.getMessage.matches(".*\\d+:\\d+.*") then
                  throw new EvalError(s"$line:$col: ${e.getMessage}")
                else throw e
          )
        for e <- argExprs do
          val prevK = argK
          val expr  = e
          argK = tailVals => More(() => evalK(expr, env, headVal => prevK(headVal :: tailVals)))
        argK(Nil)
    )

  // --- CPS apply ---
  def applyK(proc: SchemeVal, args: List[SchemeVal], k: Cont): Bounce =
    proc match
      case SchemeVal.ContinuationVal(savedState) =>
        val v                      = if args.isEmpty then SchemeVal.Void else args.head
        val (savedK, savedWinders) = savedState.asInstanceOf[(Cont, List[Winder])]
        val currentWinders         = winders
        DynamicWind.doWindK(currentWinders, savedWinders, () => savedK(v))

      case SchemeVal.CallCCVal() =>
        if args.size != 1 then throw new EvalError("call/cc: expected 1 argument")
        val savedWinders = winders
        val kontVal      = SchemeVal.ContinuationVal((k, savedWinders))
        More(() => applyK(args.head, List(kontVal), k))

      case SchemeVal.BuiltinProc(_, f) =>
        k(f(args))

      case SchemeVal.LambdaProc(params, body, closure, rest) =>
        val localEnv = bindArgs(params, rest, args, closure)
        evalBodyK(body, localEnv, k)

      case SchemeVal.CaseLambdaProc(clauses, closure) =>
        val matched = clauses.find { case (params, rest, _) =>
          rest match
            case Some(_) => args.size >= params.size
            case None    => args.size == params.size
        }
        matched match
          case Some((params, rest, body)) =>
            val localEnv = bindArgs(params, rest, args, closure)
            evalBodyK(body, localEnv, k)
          case None =>
            throw new EvalError(s"case-lambda: no matching clause for ${args.size} arguments")

      case _ => throw new EvalError(s"not a procedure: $proc")

  // --- CPS helpers ---

  private def bindArgs(params: List[String], rest: Option[String], args: List[SchemeVal], closure: Env): Env =
    rest match
      case Some(restName) =>
        if args.size < params.size then
          throw new EvalError(s"expected at least ${params.size} arguments, got ${args.size}")
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(closure))
        params.zip(args).foreach((p, a) => localEnv.define(p, a))
        localEnv.define(restName, SchemeVal.SList(args.drop(params.size)))
        localEnv
      case None =>
        if args.size != params.size then throw new EvalError(s"expected ${params.size} arguments, got ${args.size}")
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(closure))
        params.zip(args).foreach((p, a) => localEnv.define(p, a))
        localEnv

  /** Evaluate a sequence of expressions, returning the last result. */
  def evalBodyK(body: List[SchemeVal], env: Env, k: Cont): Bounce =
    body match
      case Nil          => k(SchemeVal.Void)
      case last :: Nil  => More(() => evalK(last, env, k))
      case head :: tail => More(() => evalK(head, env, _ => evalBodyK(tail, env, k)))

  /** Evaluate a list of expressions left-to-right, pass all results to k. */
  def evalListK(exprs: List[SchemeVal], env: Env, k: List[SchemeVal] => Bounce): Bounce =
    def go(remaining: List[SchemeVal], acc: List[SchemeVal]): Bounce =
      remaining match
        case Nil => k(acc.reverse)
        case head :: tail =>
          More(() => evalK(head, env, v => go(tail, v :: acc)))
    go(exprs, Nil)
