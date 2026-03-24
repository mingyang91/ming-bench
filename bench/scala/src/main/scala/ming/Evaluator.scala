package ming

/** Scheme interpreter entry point — CPS-based with first-class continuations. */
object Evaluator:

  /** Value type hierarchy. */
  sealed abstract class Val

  // CPS trampoline types (declared before Val subtypes that reference them)
  type Cont = Val => Bounce

  sealed trait Bounce
  case class BDone(v: Val)              extends Bounce
  case class BMore(thunk: () => Bounce) extends Bounce

  /** Thrown when a continuation is invoked from non-CPS code (inside a Builtin). */
  class ContinuationJump(val bounce: Bounce) extends Throwable(null, null, true, false)

  /** Thrown by `raise` to signal a Scheme exception. */
  class SchemeRaise(val value: Val) extends Throwable(null, null, true, false)

  object Val:
    case class Num(n: Long)                            extends Val
    case class Bool(b: Boolean)                        extends Val
    case class Str(chars: Array[Char])                 extends Val
    case class Symbol(name: String)                    extends Val
    case object Nil                                    extends Val
    case class SchemeChar(c: scala.Char)               extends Val
    case object Void                                   extends Val
    case class Rational(num: Long, den: Long)          extends Val
    case class Inexact(d: Double)                      extends Val
    case class Builtin(f: List[Val] => Val)            extends Val
    case class MacroTransformer(expand: Val => Val)    extends Val
    case class Record(tag: String, fields: Array[Val]) extends Val
    case class Vector(elems: Array[Val])               extends Val

    case class Closure(
      params: List[String],
      restParam: Option[String],
      body: List[Val],
      closureEnv: Env
    ) extends Val

    /** Mutable pair (cons cell). Uses reference equality. */
    class Pair(var car: Val, var cdr: Val) extends Val

    object Pair:
      def apply(car: Val, cdr: Val): Pair    = new Pair(car, cdr)
      def unapply(p: Pair): Some[(Val, Val)] = Some((p.car, p.cdr))

    /** A captured continuation (first-class), with saved winding stack. */
    case class ContinuationVal(k: Cont, winds: List[(Val, Val)]) extends Val

    /** Multiple return values (from `values`). */
    case class MultipleValues(vals: List[Val]) extends Val

    /** The call/cc primitive as a first-class value. */
    case object CallCCVal extends Val

  import Val.*

  private[ming] def mkStr(s: String): Val = Str(s.toCharArray)

  /** Create a rational, simplifying and converting to Num if denominator is 1. */
  private[ming] def mkRational(num: Long, den: Long): Val =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1 else 1
    val n    = num * sign
    val d    = den * sign
    val g    = gcd(math.abs(n), d)
    val sn   = n / g
    val sd   = d / g
    if sd == 1 then Num(sn) else Rational(sn, sd)

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  private[ming] def strValue(v: Val): String = v match
    case Str(chars) => new String(chars)
    case _          => throw new EvalError(s"not a string: ${Display.write(v)}")

  // --- Thread-local interpreter state (each thread gets its own) ---

  private val _outputBuffer: ThreadLocal[StringBuilder] =
    ThreadLocal.withInitial(() => new StringBuilder)
  private[ming] def outputBuffer: StringBuilder = _outputBuffer.get()

  private val _mutableStrings: ThreadLocal[java.util.Set[Array[Char]]] =
    ThreadLocal.withInitial(() =>
      java.util.Collections.newSetFromMap(
        new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]()
      )
    )
  private[ming] def mutableStrings: java.util.Set[Array[Char]] = _mutableStrings.get()

  private val _windingStack: ThreadLocal[List[(Val, Val)]] =
    ThreadLocal.withInitial(() => List.empty)
  private[ming] def windingStack: List[(Val, Val)] = _windingStack.get()
  private[ming] def windingStack_=(v: List[(Val, Val)]): Unit = _windingStack.set(v)

  private val _raiseHandlers: ThreadLocal[List[Val => Bounce]] =
    ThreadLocal.withInitial(() => List.empty)
  private[ming] def raiseHandlers: List[Val => Bounce] = _raiseHandlers.get()
  private[ming] def raiseHandlers_=(v: List[Val => Bounce]): Unit = _raiseHandlers.set(v)

  private val _stepLimit: ThreadLocal[Long] = ThreadLocal.withInitial(() => -1L)
  private[ming] def stepLimit: Long = _stepLimit.get()
  private[ming] def stepLimit_=(v: Long): Unit = _stepLimit.set(v)

  private val _stepCount: ThreadLocal[Long] = ThreadLocal.withInitial(() => 0L)
  private[ming] def stepCount: Long = _stepCount.get()
  private[ming] def stepCount_=(v: Long): Unit = _stepCount.set(v)

  private val _lastPos: ThreadLocal[String] = ThreadLocal.withInitial(() => "1:1")
  private[ming] def lastPos: String = _lastPos.get()
  private[ming] def lastPos_=(v: String): Unit = _lastPos.set(v)

  private[ming] def error(msg: String): Nothing =
    throw new EvalError(s"$lastPos: $msg")

  // --- Closure env setup ---
  private[ming] def setupClosureEnv(
    params: List[String],
    restParam: Option[String],
    body: List[Val],
    closureEnv: Env,
    args: List[Val]
  ): Env =
    if restParam.isDefined then
      if args.length < params.length then
        error(s"lambda: expected at least ${params.length} arguments, got ${args.length}")
    else if args.length != params.length then error(s"lambda: expected ${params.length} arguments, got ${args.length}")
    val callEnv = Env.empty(Some(closureEnv))
    params.zip(args).foreach((p, a) => callEnv.define(p, a))
    restParam.foreach { rp =>
      val restArgs = args.drop(params.length)
      callEnv.define(rp, restArgs.foldRight(Nil: Val)((a, acc) => Pair(a, acc)))
    }
    callEnv

  private[ming] def toList(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toList(cdr)
    case _              => error("improper list")

  // ======== Trampoline runner ========

  private[ming] def trampoline(b0: Bounce): Val =
    var b = b0
    while true do
      b match
        case BDone(v) => return v
        case BMore(thunk) =>
          if stepLimit >= 0 then
            stepCount = stepCount + 1
            if stepCount > stepLimit then
              throw new EvalError("step limit exceeded")
          try b = thunk()
          catch
            case jump: ContinuationJump => b = jump.bounce
            case raise: SchemeRaise =>
              if raiseHandlers.nonEmpty then
                val handler = raiseHandlers.head
                raiseHandlers = raiseHandlers.tail
                b = handler(raise.value)
              else throw new EvalError(s"unhandled exception: ${Display.write(raise.value)}")
    throw new RuntimeException("unreachable")

  // ======== CPS Evaluator core ========

  private[ming] def evalK(expr: Val, env: Env, k: Cont): Bounce = BMore { () =>
    expr match
      case _: Num | _: Bool | _: Str | _: SchemeChar | _: Builtin | _: MacroTransformer | _: Rational | _: Inexact |
          _: Record | _: Vector | _: Closure | _: ContinuationVal | CallCCVal =>
        k(expr)
      case Nil  => k(Nil)
      case Void => k(Void)
      case Symbol(name) =>
        k(env.lookup(name).getOrElse(error(s"unbound variable: $name")))
      case Pair(Symbol("quote"), Pair(datum, Nil)) => k(datum)
      case Pair(Symbol("quasiquote"), Pair(template, Nil)) =>
        Quasiquote.evalQuasiquoteK(template, env, k)
      case Pair(Symbol("define"), rest) => SpecialForms.evalDefineK(rest, env, k)
      case Pair(Symbol("set!"), Pair(Symbol(name), Pair(valueExpr, Nil))) =>
        evalK(
          valueExpr,
          env,
          v =>
            if !env.set(name, v) then error(s"unbound variable: $name")
            k(Void)
        )
      case Pair(Symbol("if"), rest)                 => SpecialForms.evalIfK(rest, env, k)
      case Pair(Symbol("lambda"), rest)             => k(SpecialForms.evalLambda(rest, env))
      case Pair(Symbol("begin"), body)              => evalSeqK(toList(body), env, k)
      case Pair(Symbol("cond"), clauses)            => SpecialForms.evalCondK(clauses, env, k)
      case Pair(Symbol("let"), rest)                => BindingForms.evalLetK(rest, env, k)
      case Pair(Symbol("and"), args)                => SpecialForms.evalAndK(toList(args), env, k)
      case Pair(Symbol("or"), args)                 => SpecialForms.evalOrK(toList(args), env, k)
      case Pair(Symbol("define-record-type"), rest) => k(Records.evalDefineRecordType(rest, env, error))
      case Pair(Symbol("case-lambda"), clausesList) => k(SpecialForms.evalCaseLambda(clausesList, env))
      case Pair(Symbol("let*"), rest)               => BindingForms.evalLetStarK(rest, env, k)
      case Pair(Symbol("letrec"), rest)             => BindingForms.evalLetrecK(rest, env, k)
      case Pair(Symbol("letrec*"), rest)            => BindingForms.evalLetrecStarK(rest, env, k)
      case Pair(Symbol("case"), rest)               => SpecialForms.evalCaseK(rest, env, k)
      case Pair(Symbol("do"), rest)                 => BindingForms.evalDoK(rest, env, k)
      case Pair(Symbol("dynamic-wind"), Pair(inExpr, Pair(bodyExpr, Pair(outExpr, Nil)))) =>
        WindGuard.evalDynamicWindK(inExpr, bodyExpr, outExpr, env, k)
      case Pair(Symbol("guard"), Pair(Pair(Symbol(varName), clauses), body)) =>
        WindGuard.evalGuardK(varName, clauses, body, env, k)
      case Pair(Symbol("with-exception-handler"), Pair(handlerExpr, Pair(thunkExpr, Nil))) =>
        WindGuard.evalWithExceptionHandlerK(handlerExpr, thunkExpr, env, k)
      case Pair(Symbol("call-with-values"), Pair(producerExpr, Pair(consumerExpr, Nil))) =>
        evalK(
          producerExpr,
          env,
          producer =>
            evalK(
              consumerExpr,
              env,
              consumer =>
                applyK(
                  producer,
                  List.empty,
                  prodResult =>
                    prodResult match
                      case MultipleValues(vals) => applyK(consumer, vals, k)
                      case single               => applyK(consumer, List(single), k)
                )
            )
        )
      case Pair(Symbol("define-syntax"), Pair(Symbol(name), Pair(sr, Nil))) =>
        k(Macros.evalDefineSyntax(name, sr, env))
      case Pair(Symbol("syntax-case"), rest) =>
        SyntaxCase.evalSyntaxCaseK(rest, env, k)
      case Pair(Symbol("syntax"), Pair(template, Nil)) =>
        k(SyntaxCase.expandSyntaxTemplate(template, env))
      case Pair(Symbol("with-syntax"), rest) =>
        SyntaxCase.evalWithSyntaxK(rest, env, k)
      case Pair(Symbol(name), pArgs) =>
        env.lookup(name) match
          case Some(MacroTransformer(expand)) => evalK(expand(expr), env, k)
          case Some(func) =>
            evalListK(toList(pArgs), env, argList => applyK(func, argList, k))
          case None => error(s"unbound variable: $name")
      case Pair(head, args) =>
        evalK(head, env, func => evalListK(toList(args), env, argList => applyK(func, argList, k)))
      case _ => k(expr)
  }

  // ======== Argument list evaluation (right-to-left for correct call/cc semantics) ========

  private def evalListK(exprs: List[Val], env: Env, k: List[Val] => Bounce): Bounce =
    val reversed = exprs.reverse
    def loop(remaining: List[Val], acc: List[Val]): Bounce =
      remaining match
        case scala.Nil => k(acc)
        case head :: tail =>
          evalK(head, env, v => BMore(() => loop(tail, v :: acc)))
    loop(reversed, scala.Nil)

  // ======== Sequence evaluation (left-to-right, last in tail position) ========

  private[ming] def evalSeqK(exprs: List[Val], env: Env, k: Cont): Bounce =
    exprs match
      case scala.Nil         => k(Void)
      case last :: scala.Nil => evalK(last, env, k)
      case head :: tail      => evalK(head, env, _ => BMore(() => evalSeqK(tail, env, k)))

  // ======== Function application (CPS) ========

  private[ming] def applyBuiltinChecked(f: List[Val] => Val, args: List[Val]): Val =
    try f(args)
    catch
      case e: EvalError =>
        if !e.getMessage.matches(".*\\d+:\\d+.*") then error(e.getMessage)
        else throw e

  private[ming] def applyK(func: Val, args: List[Val], k: Cont): Bounce =
    func match
      case CallCCVal =>
        args match
          case List(proc) => applyK(proc, List(ContinuationVal(k, windingStack)), k)
          case _          => error("call/cc requires 1 argument")
      case ContinuationVal(savedK, savedWinds) =>
        val v = args match
          case List(single) => single
          case _            => MultipleValues(args)
        val currentWinds = windingStack
        val commonLen    = WindGuard.commonWindTailLength(currentWinds, savedWinds)
        val toUnwind     = currentWinds.take(currentWinds.length - commonLen).map(_._2)
        val toRewind     = savedWinds.take(savedWinds.length - commonLen).reverse.map(_._1)
        WindGuard.runThunks(
          toUnwind,
          BMore { () =>
            WindGuard.runThunks(
              toRewind,
              BMore { () =>
                windingStack = savedWinds
                savedK(v)
              }
            )
          }
        )
      case Closure(params, restParam, body, closureEnv) =>
        val callEnv = setupClosureEnv(params, restParam, body, closureEnv, args)
        evalSeqK(body, callEnv, k)
      case Builtin(f) =>
        try k(applyBuiltinChecked(f, args))
        catch case jump: ContinuationJump => jump.bounce
      case _ => error(s"not a procedure: ${Display.write(func)}")

  // ======== Public API forwarding (for munit tests) ========

  def evalStr(input: String): String = Interpreter.evalStr(input)

  def evalStrWithOutput(input: String): (String, String) = Interpreter.evalStrWithOutput(input)

  // ======== Step-limited evaluation ========

  def evalStrWithLimit(input: String, maxSteps: Long): String =
    stepLimit = maxSteps
    stepCount = 0L
    try
      val result = Interpreter.runProgram(input)
      Display.write(result)
    finally
      stepLimit = -1L
      stepCount = 0L
