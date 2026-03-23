package ming

import Value.*
import Expr.*
import EvalHelpers.{evalError, evalLambda, evalQuote, parseParams, valueToList}
import scala.compiletime.uninitialized

/** CPS interpreter with trampoline for tail calls and first-class continuations. */
object Evaluator extends EvalForms with EvalWind with EvalExceptions with EvalSyntaxCaseForms with EvalApply:
  type K = Value => Bounce

  private var depth            = 0 // amortized trampolining
  private val MaxDepth         = 128
  protected val pendingReturns = new java.util.IdentityHashMap[Expr, Value]()

  case class WindEntry(inThunk: Value, outThunk: Value)
  protected var windStack: List[WindEntry] = Nil

  case class HandlerEntry(handler: Value => Bounce, windAtInstall: List[WindEntry])
  protected var handlerStack: List[HandlerEntry] = Nil

  protected var bodyRemaining: List[Expr] = Nil // call/cc body-restart context
  protected var bodyEnvRef: Env           = uninitialized
  protected var bodyK: K                  = uninitialized

  // syntax-case: current bindings for the `syntax` form
  protected var currentSyntaxBindings: Map[String, SyntaxCase.Binding] = Map.empty
  protected var currentSyntaxEnvs: (Env, Env)                          = (Env(), Env())

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
    windStack = Nil
    handlerStack = Nil
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    run(evalBody(exprs, env, v => Bounce.Done(v))).display

  def evalStrWithOutput(input: String): (String, String) =
    depth = 0
    pendingReturns.clear()
    windStack = Nil
    handlerStack = Nil
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val output = new StringBuilder
    val env    = makeGlobalEnv(output)
    val result = run(evalBody(exprs, env, v => Bounce.Done(v)))
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env                         = Env()
    val dummy: List[Value] => Value = _ => throw new EvalError("internal: direct call to special")
    Builtins.register(env, output)
    env.define("call/cc", BuiltinVal("call/cc", dummy))
    env.define("call-with-current-continuation", BuiltinVal("call-with-current-continuation", dummy))
    env.define("apply", BuiltinVal("apply", dummy))
    env.define("map", BuiltinVal("map", dummy))
    env.define("dynamic-wind", BuiltinVal("dynamic-wind", dummy))
    env.define("raise", BuiltinVal("raise", dummy))
    env.define("with-exception-handler", BuiltinVal("with-exception-handler", dummy))
    env.define("call-with-values", BuiltinVal("call-with-values", dummy))
    env.define("for-each", BuiltinVal("for-each", dummy))
    env

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
      case Num(n, _)                                       => k(IntVal(n))
      case Rat(n, d, _)                                    => k(Value.makeRational(n, d))
      case Flt(d, _)                                       => k(FloatVal(d))
      case Bool(b, _)                                      => k(BoolVal(b))
      case Str(s, _)                                       => k(StrVal(s.toCharArray))
      case Chr(c, _)                                       => k(CharVal(c))
      case Sym(name, pos)                                  => k(env.lookup(name, pos))
      case SList(Nil, pos)                                 => evalError("empty application", pos)
      case SList(Sym("if", _) :: args, pos)                => evalIf(args, env, pos, k)
      case SList(Sym("define", _) :: args, pos)            => evalDefineCps(args, env, pos, k)
      case SList(Sym("set!", _) :: args, pos)              => evalSetBang(args, env, pos, k)
      case SList(Sym("quote", _) :: args, pos)             => k(evalQuote(args, pos))
      case SList(Sym("quasiquote", _) :: tmpl :: Nil, pos) => evalQuasiquote(tmpl, env, k)
      case SList(Sym("lambda", _) :: args, pos)            => k(evalLambda(args, env, pos))
      case SList(Sym("case-lambda", _) :: clauses, pos)    => k(evalCaseLambda(clauses, env, pos))
      case SList(Sym("let", _) :: args, pos)               => evalLetCps(args, env, pos, k)
      case SList(Sym("let*", _) :: args, pos)              => evalLetStarCps(args, env, pos, k)
      case SList(Sym("letrec", _) :: args, pos)            => evalLetrecCps(args, env, pos, k)
      case SList(Sym("letrec*", _) :: args, pos)           => evalLetrecStarCps(args, env, pos, k)
      case SList(Sym("case", _) :: keyExpr :: clauses, pos) =>
        eval(keyExpr, env, keyVal => evalCaseCps(keyVal, clauses, env, pos, k))
      case SList(Sym("do", _) :: args, pos)                 => evalDoCps(args, env, pos, k)
      case SList(Sym("begin", _) :: args, pos)              => evalBegin(args, pos, env, k)
      case SList(Sym("cond", _) :: clauses, pos)            => evalCondCps(clauses, env, pos, k)
      case SList(Sym("and", _) :: args, _)                  => evalAndCps(args, env, k)
      case SList(Sym("or", _) :: args, _)                   => evalOrCps(args, env, k)
      case SList(Sym("guard", _) :: args, pos)              => evalGuardForm(args, env, pos, k)
      case SList(Sym("define-record-type", _) :: args, pos) => evalDefineRecordType(args, env, pos, k)
      case SList(Sym("define-syntax", _) :: rest, pos)      => evalDefineSyntax(rest, env, pos, k)
      case SList(Sym("syntax-case", _) :: scrutineeExpr :: SList(literals, _) :: clauses, pos) =>
        eval(scrutineeExpr, env, scrutinee => evalSyntaxCaseClauses(scrutinee, literals, clauses, env, pos, k))
      case SList(Sym("syntax", _) :: template :: Nil, _) =>
        k(SyntaxCase.instantiateTemplate(template, currentSyntaxBindings, currentSyntaxEnvs._1, currentSyntaxEnvs._2))
      case SList(Sym("with-syntax", _) :: SList(bindingExprs, _) :: body, pos) =>
        evalWithSyntax(bindingExprs, body, env, pos, k)
      case callccExpr @ SList(Sym(name, _) :: procExpr :: Nil, pos)
          if name == "call/cc" || name == "call-with-current-continuation" =>
        evalCallCc(callccExpr, procExpr, env, pos, k)
      case SList(Sym(name, symPos) :: args, pos) if isMacro(name, env) =>
        expandMacro(name, symPos, args, expr, env, pos, k)
      case SList(head :: args, pos) =>
        eval(head, env, proc => evalArgs(args, env, values => applyProc(proc, values, pos, k)))

  private def evalIf(args: List[Expr], env: Env, pos: Option[Pos], k: K): Bounce =
    args match
      case cond :: thenB :: elseB :: Nil =>
        eval(cond, env, cv => if cv.isTruthy then tailEval(thenB, env, k) else tailEval(elseB, env, k))
      case cond :: thenB :: Nil =>
        eval(cond, env, cv => if cv.isTruthy then tailEval(thenB, env, k) else k(VoidVal))
      case _ => evalError("if: bad syntax", pos)

  private def evalSetBang(args: List[Expr], env: Env, pos: Option[Pos], k: K): Bounce =
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

  private def evalQuasiquote(tmpl: Expr, env: Env, k: K): Bounce =
    tmpl match
      case SList(Sym("unquote", _) :: expr :: Nil, _) =>
        eval(expr, env, k)
      case SList(elems, pos) =>
        evalQQList(elems, env, vs => k(vs.foldRight(NilVal: Value)((v, acc) => Pair(v, acc))))
      case DottedList(heads, tail, _) =>
        evalQQList(
          heads,
          env,
          hvs =>
            evalQuasiquote(
              tail,
              env,
              tv => k(hvs.foldRight(tv)((v, acc) => Pair(v, acc)))
            )
        )
      case _ => k(EvalHelpers.exprToValue(tmpl))

  private def evalQQList(elems: List[Expr], env: Env, k: List[Value] => Bounce): Bounce =
    elems match
      case Nil => k(Nil)
      case SList(Sym("unquote-splicing", _) :: expr :: Nil, _) :: rest =>
        eval(
          expr,
          env,
          spliced =>
            val splicedList = EvalHelpers.valueToList(spliced)
            evalQQList(rest, env, vs => k(splicedList ++ vs))
        )
      case head :: rest =>
        evalQuasiquote(head, env, v => evalQQList(rest, env, vs => k(v :: vs)))

  private def evalBegin(args: List[Expr], pos: Option[Pos], env: Env, k: K): Bounce =
    if args.isEmpty then evalError("begin: empty", pos)
    evalBody(args, env, k)

  private def evalGuardForm(args: List[Expr], env: Env, pos: Option[Pos], k: K): Bounce =
    args match
      case SList(Sym(variable, _) :: clauses, _) :: body if body.nonEmpty =>
        evalGuard(variable, clauses, body, env, pos, k)
      case _ => evalError("guard: bad syntax", pos)

  private def evalDefineSyntax(rest: List[Expr], env: Env, pos: Option[Pos], k: K): Bounce =
    rest match
      case Sym(name, _) :: SList(Sym("syntax-rules", _) :: srArgs, _) :: Nil =>
        val (literals, rules) = Macro.parseSyntaxRules(srArgs, pos)
        env.define(name, Value.MacroVal(literals, rules, env))
        k(BoolVal(true))
      case Sym(name, _) :: transformerExpr :: Nil =>
        eval(
          transformerExpr,
          env,
          { transformer =>
            env.define(name, Value.ProcMacroVal(transformer, env))
            k(BoolVal(true))
          }
        )
      case _ => evalError("define-syntax: bad syntax", pos)

  private def isMacro(name: String, env: Env): Boolean =
    env.lookupOption(name).exists(v => v.isInstanceOf[Value.MacroVal] || v.isInstanceOf[Value.ProcMacroVal])

  private def expandMacro(
    name: String,
    symPos: Option[Pos],
    args: List[Expr],
    expr: Expr,
    env: Env,
    pos: Option[Pos],
    k: K
  ): Bounce =
    env.lookup(name, symPos) match
      case Value.MacroVal(literals, rules, defEnv) =>
        tailEval(Macro.expand(name, args, literals, rules, defEnv, env, pos), env, k)
      case Value.ProcMacroVal(transformer, _) =>
        applyProc(
          transformer,
          List(EvalHelpers.exprToValue(expr)),
          pos,
          resultValue => tailEval(SyntaxCase.valueToExpr(resultValue), env, k)
        )
      case _ => evalError(s"$name: not a macro", pos)

  private def evalCaseLambda(clauses: List[Expr], env: Env, pos: Option[Pos]): Value =
    val parsed = clauses.map {
      case SList(SList(params, _) :: body, _) if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params, "case-lambda", pos)
        (paramNames, restParam, body)
      case SList(DottedList(params, Sym(restName, _), _) :: body, _) if body.nonEmpty =>
        val paramNames = params.map {
          case Sym(p, _) => p
          case _         => evalError("case-lambda: non-symbol parameter", pos)
        }
        (paramNames, Some(restName), body)
      case SList(Sym(restName, _) :: body, _) if body.nonEmpty =>
        (Nil, Some(restName), body)
      case _ => evalError("case-lambda: bad clause syntax", pos)
    }
    CaseLambdaVal(parsed, env)
