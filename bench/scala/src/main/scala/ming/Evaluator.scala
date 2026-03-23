package ming

import Value.*
import Expr.*
import EvalHelpers.{evalError, evalLambda, evalQuote, parseParams, valueToList}
import scala.compiletime.uninitialized

/** CPS interpreter with trampoline for tail calls and first-class continuations. */
object Evaluator extends EvalForms:
  type K = Value => Bounce

  // Depth counter for amortized trampolining
  private var depth    = 0
  private val MaxDepth = 128

  // Pending returns for body-restart continuations
  private val pendingReturns = new java.util.IdentityHashMap[Expr, Value]()

  // Current body context, captured by call/cc for body-restart
  private var bodyRemaining: List[Expr] = Nil
  private var bodyEnvRef: Env           = uninitialized
  private var bodyK: K                  = uninitialized

  protected def trampoline(thunk: => Bounce): Bounce =
    depth += 1
    if depth >= MaxDepth then Bounce.More(() => thunk)
    else thunk

  private def tailEval(expr: Expr, env: Env, k: K): Bounce =
    depth += 1
    if depth >= MaxDepth then Bounce.TailEval(expr, env, k)
    else eval(expr, env, k)

  protected def tailBody(exprs: List[Expr], env: Env, k: K): Bounce =
    depth += 1
    if depth >= MaxDepth then Bounce.TailBody(exprs, env, k)
    else evalBody(exprs, env, k)

  private def run(initial: Bounce): Value =
    var current = initial
    while true do
      current match
        case Bounce.Done(v) => return v
        case Bounce.More(thunk) =>
          depth = 0; current = thunk()
        case Bounce.TailEval(expr, env, k) =>
          depth = 0; current = eval(expr, env, k)
        case Bounce.TailBody(exprs, env, k) =>
          depth = 0; current = evalBody(exprs, env, k)
    throw new AssertionError("unreachable")

  def evalStr(input: String): String =
    depth = 0
    pendingReturns.clear()
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    run(evalBody(exprs, env, v => Bounce.Done(v))).display

  def evalStrWithOutput(input: String): (String, String) =
    depth = 0
    pendingReturns.clear()
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val output = new StringBuilder
    val env    = makeGlobalEnv(output)
    val result = run(evalBody(exprs, env, v => Bounce.Done(v)))
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env = Env()
    Builtins.register(env, output)
    registerSpecials(env)
    env

  private def registerSpecials(env: Env): Unit =
    val dummy: List[Value] => Value = _ => throw new EvalError("internal: direct call to special")
    env.define("call/cc", BuiltinVal("call/cc", dummy))
    env.define("call-with-current-continuation", BuiltinVal("call-with-current-continuation", dummy))
    env.define("apply", BuiltinVal("apply", dummy))
    env.define("map", BuiltinVal("map", dummy))

  protected def evalBody(exprs: List[Expr], env: Env, k: K): Bounce =
    bodyRemaining = exprs
    bodyEnvRef = env
    bodyK = k
    exprs match
      case Nil         => throw new EvalError("empty sequence")
      case last :: Nil => eval(last, env, k)
      case head :: tail =>
        eval(head, env, _ => tailBody(tail, env, k))

  protected def eval(expr: Expr, env: Env, k: K): Bounce =
    expr match
      case Num(n, _)       => k(IntVal(n))
      case Bool(b, _)      => k(BoolVal(b))
      case Str(s, _)       => k(StrVal(s.toCharArray))
      case Chr(c, _)       => k(CharVal(c))
      case Sym(name, pos)  => k(env.lookup(name, pos))
      case SList(Nil, pos) => evalError("empty application", pos)

      case SList(Sym("if", _) :: args, pos) =>
        args match
          case cond :: thenB :: elseB :: Nil =>
            eval(
              cond,
              env,
              cv =>
                if cv.isTruthy then tailEval(thenB, env, k)
                else tailEval(elseB, env, k)
            )
          case cond :: thenB :: Nil =>
            eval(
              cond,
              env,
              cv =>
                if cv.isTruthy then tailEval(thenB, env, k)
                else k(BoolVal(false))
            )
          case _ => evalError("if: bad syntax", pos)

      case SList(Sym("define", _) :: args, pos) =>
        evalDefineCps(args, env, pos, k)

      case SList(Sym("set!", _) :: args, pos) =>
        args match
          case Sym(name, _) :: valExpr :: Nil =>
            eval(
              valExpr,
              env,
              { v =>
                env.set(name, v, pos); k(BoolVal(true))
              }
            )
          case _ => evalError("set!: bad syntax", pos)

      case SList(Sym("quote", _) :: args, pos) =>
        k(evalQuote(args, pos))

      case SList(Sym("lambda", _) :: args, pos) =>
        k(evalLambda(args, env, pos))

      case SList(Sym("let", _) :: args, pos) =>
        evalLetCps(args, env, pos, k)

      case SList(Sym("begin", _) :: args, pos) =>
        if args.isEmpty then evalError("begin: empty", pos)
        evalBody(args, env, k)

      case SList(Sym("cond", _) :: clauses, pos) =>
        evalCondCps(clauses, env, pos, k)

      case SList(Sym("and", _) :: args, _) =>
        evalAndCps(args, env, k)

      case SList(Sym("or", _) :: args, _) =>
        evalOrCps(args, env, k)

      case SList(Sym("not", _) :: args, _) =>
        if args.length != 1 then throw new EvalError("not: expected 1 argument")
        eval(args.head, env, v => k(BoolVal(!v.isTruthy)))

      case SList(Sym("define-syntax", _) :: Sym(name, _) :: SList(Sym("syntax-rules", _) :: srArgs, _) :: Nil, pos) =>
        val (literals, rules) = Macro.parseSyntaxRules(srArgs, pos)
        env.define(name, Value.MacroVal(literals, rules, env))
        k(BoolVal(true))

      case callccExpr @ SList(Sym(name, _) :: procExpr :: Nil, pos)
          if name == "call/cc" || name == "call-with-current-continuation" =>
        evalCallCc(callccExpr, procExpr, env, pos, k)

      // Macro expansion
      case SList(Sym(name, symPos) :: args, pos) if env.lookupOption(name).exists(_.isInstanceOf[Value.MacroVal]) =>
        val Value.MacroVal(literals, rules, defEnv) = env.lookup(name, symPos): @unchecked
        val expanded                                = Macro.expand(name, args, literals, rules, defEnv, env, pos)
        tailEval(expanded, env, k)

      // General function application
      case SList(head :: args, pos) =>
        eval(head, env, proc => evalArgs(args, env, values => applyProc(proc, values, pos, k)))

  private def evalArgs(args: List[Expr], env: Env, k: List[Value] => Bounce): Bounce =
    args match
      case Nil => k(Nil)
      case head :: tail =>
        eval(
          head,
          env,
          v =>
            depth += 1
            if depth >= MaxDepth then Bounce.More(() => evalArgs(tail, env, vs => k(v :: vs)))
            else evalArgs(tail, env, vs => k(v :: vs))
        )

  private def applyProc(proc: Value, values: List[Value], pos: Option[Pos], k: K): Bounce =
    proc match
      case LambdaVal(params, restParam, body, closure) =>
        val localEnv = closure.extendWithRest(params, restParam, values)
        tailBody(body, localEnv, k)

      case ContinuationVal(invoke) =>
        if values.length != 1 then evalError("continuation: expected 1 argument", pos)
        invoke(values.head)

      // call/cc used as a value — CPS escape continuation
      case BuiltinVal(name, _) if name == "call/cc" || name == "call-with-current-continuation" =>
        if values.length != 1 then evalError("call/cc: expected 1 argument", pos)
        val contVal = ContinuationVal(v => Bounce.More(() => k(v)))
        applyProc(values.head, List(contVal), pos, k)

      case BuiltinVal("apply", _) =>
        applyBuiltinApply(values, pos, k)

      case BuiltinVal("map", _) =>
        applyBuiltinMap(values, pos, k)

      case BuiltinVal(name, fn) =>
        try k(fn(values))
        catch
          case e: EvalError =>
            if e.getMessage.matches(".*\\d+:\\d+.*") then throw e
            else evalError(e.getMessage, pos)
          case e: ArithmeticException =>
            evalError(e.getMessage, pos)

      case _ => evalError("not a procedure", pos)

  private def applyBuiltinApply(args: List[Value], pos: Option[Pos], k: K): Bounce =
    if args.length < 2 then evalError("apply: expected at least 2 arguments", pos)
    val proc       = args.head
    val prefixArgs = args.slice(1, args.length - 1).toList
    val trailing   = valueToList(args.last)
    applyProc(proc, prefixArgs ++ trailing, pos, k)

  private def applyBuiltinMap(args: List[Value], pos: Option[Pos], k: K): Bounce =
    if args.length < 2 then evalError("map: expected at least 2 arguments", pos)
    val proc  = args.head
    val lists = args.tail.map(EvalHelpers.valueToList)
    mapLoop(proc, lists, Nil, pos, k)

  private def mapLoop(
    proc: Value,
    lists: List[List[Value]],
    acc: List[Value],
    pos: Option[Pos],
    k: K
  ): Bounce =
    if lists.head.isEmpty then
      val result = acc.reverse.foldRight(Value.NilVal: Value)((v, t) => Value.PairVal(v, t))
      k(result)
    else
      val heads = lists.map(_.head)
      val tails = lists.map(_.tail)
      applyProc(
        proc,
        heads,
        pos,
        v =>
          depth += 1
          if depth >= MaxDepth then Bounce.More(() => mapLoop(proc, tails, v :: acc, pos, k))
          else mapLoop(proc, tails, v :: acc, pos, k)
      )

  /** call/cc as special form — hybrid: CPS for escape, body-restart for reentrant. */
  private def evalCallCc(callccExpr: Expr, procExpr: Expr, env: Env, pos: Option[Pos], k: K): Bounce =
    val pending = pendingReturns.remove(callccExpr)
    if pending != null then k(pending)
    else
      val capturedRemaining = bodyRemaining
      val capturedEnv       = bodyEnvRef
      val capturedK         = bodyK
      val cpsK              = k
      var active            = true
      eval(
        procExpr,
        env,
        { proc =>
          val contVal = ContinuationVal { v =>
            if active then
              active = false
              Bounce.More(() => cpsK(v))
            else
              pendingReturns.put(callccExpr, v)
              tailBody(capturedRemaining, capturedEnv, capturedK)
          }
          applyProc(
            proc,
            List(contVal),
            pos,
            { procResult =>
              active = false
              k(procResult)
            }
          )
        }
      )
