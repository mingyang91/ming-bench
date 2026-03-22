package ming

import SchemeValue.*

/** Scheme interpreter with CPS trampoline for full call/cc support. */
object Evaluator:

  // --- CPS Trampoline types (package-visible for helper objects) ---
  sealed private[ming] trait Step
  private[ming] case class EvalS(expr: SchemeValue, env: Env, k: Kont, out: String) extends Step
  private[ming] case class ReturnS(value: SchemeValue, k: Kont, out: String)        extends Step
  private[ming] case class DoneS(value: SchemeValue, out: String)                   extends Step
  private[ming] case class RaiseS(value: SchemeValue, k: Kont, out: String)         extends Step

  // --- Public API ---

  def evalStr(input: String): String =
    evalStrWithOutput(input)._1

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val (value, output) = evalProgram(exprs, Env.empty)
    (value.display, output)

  /** Legacy eval used by LetrecFrame.init(). */
  private[ming] def eval(expr: SchemeValue, env: Env): SchemeValue =
    run(EvalS(expr, env, Kont.Halt, ""))._1

  // --- Program evaluation ---

  private def evalProgram(
    exprs: List[SchemeValue],
    env: Env
  ): (SchemeValue, String) =
    val (defines, body) = SpecialForms.collectDefines(exprs, Nil)
    if defines.isEmpty then run(SpecialForms.startBody(body, env, Kont.Halt, ""))
    else
      val frame = new Env.LetrecFrame(defines, env)
      frame.initFunctions()
      val varInits = defines.collect { case (name, None, List(valueExpr)) =>
        SchemeList(List(SchemeSymbol("set!"), SchemeSymbol(name), valueExpr))
      }
      run(SpecialForms.startBody(varInits ++ body, frame, Kont.Halt, ""))

  // --- Trampoline ---

  @scala.annotation.tailrec
  private def run(step: Step): (SchemeValue, String) = step match
    case DoneS(v, o)            => (v, o)
    case EvalS(expr, env, k, o) => run(evalStep(expr, env, k, o))
    case ReturnS(v, k, o)       => run(KontApply.applyKont(v, k, o))
    case RaiseS(v, k, o)        => run(ExceptionHandling.handleRaise(v, k, o))

  // --- Single evaluation step ---

  private def evalStep(
    expr: SchemeValue,
    env: Env,
    k: Kont,
    out: String
  ): Step = expr match
    case SchemeSymbol(name) =>
      val v = env.get(name) match
        case Some(v) => v
        case None =>
          if isKnownName(name) then SchemeBuiltinProc(name)
          else throw new EvalError(s"unbound variable: $name", expr.pos)
      ReturnS(v, k, out)
    case rs: SchemeResolvedSymbol =>
      val v = rs.resolveEnv.get(rs.name) match
        case Some(v) => v
        case None    => throw new EvalError(s"unbound variable: ${rs.name}")
      ReturnS(v, k, out)
    case SchemeList(Nil) =>
      throw new EvalError("empty application", expr.pos)
    case SchemeList(SchemeSymbol(op) :: args) if isSpecialForm(op) =>
      try SpecialForms.evalSpecial(op, args, env, k, out)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case SchemeList(head :: args) =>
      lookupMacro(head, env) match
        case Some(m) =>
          EvalS(Macros.expand(m, head :: args), env, k, out)
        case None =>
          try EvalS(head, env, Kont.EvalOp(args, env, k, expr.pos), out)
          catch
            case e: EvalError if e.sourcePos == SourcePos.None =>
              throw new EvalError(e.baseMessage, expr.pos)
    case _ =>
      ReturnS(expr, k, out)

  // --- Dispatch helpers ---

  private def isSpecialForm(op: String): Boolean = op match
    case "define" | "if" | "quote" | "lambda" | "and" | "or" | "let" | "let*" | "letrec" | "letrec*" | "begin" |
        "cond" | "case" | "do" | "set!" | "call/cc" | "call-with-current-continuation" | "define-syntax" | "when" |
        "guard" =>
      true
    case _ => false

  private[ming] def isKnownName(name: String): Boolean =
    Builtins.knownNames.contains(name) ||
      name == "call/cc" || name == "call-with-current-continuation"

  // --- Shared helpers ---

  private[ming] def isFalsy(v: SchemeValue): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private[ming] def updateSeqEnv(k: Kont, env: Env): Kont = k match
    case Kont.Seq(remaining, _, nextK) => Kont.Seq(remaining, env, nextK)
    case other                         => other

  private def lookupMacro(head: SchemeValue, env: Env): Option[SchemeMacro] =
    head match
      case SchemeSymbol(name) =>
        env.get(name) match
          case Some(m: SchemeMacro) => Some(m)
          case _                    => None
      case _ => None
