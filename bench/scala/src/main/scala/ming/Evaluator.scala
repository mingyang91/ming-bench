package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. */
object Evaluator:

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    val env    = Env.default()
    val result = exprs.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))
    SchemeVal.display(result)

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    val output = new StringBuilder
    val env    = Env.defaultWithOutput(output)
    val result = exprs.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))
    (SchemeVal.display(result), output.toString)

  /** Trampoline: repeatedly evaluate until result is not a TailCall. */
  def eval(expr: SchemeVal, env: Env): SchemeVal =
    var result = evalStep(expr, env)
    while result.isInstanceOf[SchemeVal.TailCall] do
      val tc = result.asInstanceOf[SchemeVal.TailCall]
      result = evalStep(tc.expr, tc.env)
    result

  /** Evaluate body expressions, returning TailCall for the last one. */
  def evalBodyTail(body: List[SchemeVal], env: Env): SchemeVal =
    if body.isEmpty then SchemeVal.Void
    else
      body.init.foreach(e => eval(e, env))
      SchemeVal.TailCall(body.last, env)

  private def evalStep(expr: SchemeVal, env: Env): SchemeVal =
    try
      expr match
        case SchemeVal.IntVal(_)              => expr
        case SchemeVal.RationalVal(_, _)      => expr
        case SchemeVal.FloatVal(_)            => expr
        case SchemeVal.BoolVal(_)             => expr
        case SchemeVal.StringVal(_)           => expr
        case SchemeVal.CharVal(_)             => expr
        case SchemeVal.BuiltinProc(_, _)      => expr
        case SchemeVal.LambdaProc(_, _, _, _) => expr
        case SchemeVal.CaseLambdaProc(_, _)   => expr
        case SchemeVal.MacroVal(_, _, _, _)   => expr
        case SchemeVal.VectorVal(_)           => expr
        case SchemeVal.Symbol(name) =>
          env.lookup(name) match
            case Some(v) => v
            case None    => throw new EvalError(s"unbound variable: $name")
        case SchemeVal.SList(elems) if elems.isEmpty =>
          throw new EvalError("empty application")
        case SchemeVal.SList(elems) =>
          elems.head match
            case SchemeVal.Symbol("define") => evalDefine(elems.tail, env)
            case SchemeVal.Symbol("if")     => evalIf(elems.tail, env)
            case SchemeVal.Symbol("quote") =>
              if elems.tail.size != 1 then throw new EvalError("quote: expected 1 argument")
              elems.tail.head
            case SchemeVal.Symbol("lambda")        => evalLambda(elems.tail, env)
            case SchemeVal.Symbol("and")           => evalAnd(elems.tail, env)
            case SchemeVal.Symbol("or")            => evalOr(elems.tail, env)
            case SchemeVal.Symbol("begin")         => evalBegin(elems.tail, env)
            case SchemeVal.Symbol("let")           => BindingForms.evalLet(elems.tail, env)
            case SchemeVal.Symbol("cond")          => BindingForms.evalCond(elems.tail, env)
            case SchemeVal.Symbol("set!")          => evalSet(elems.tail, env)
            case SchemeVal.Symbol("define-syntax") => evalDefineSyntax(elems.tail, env)
            case SchemeVal.Symbol("case-lambda")   => evalCaseLambda(elems.tail, env)
            case SchemeVal.Symbol("letrec")        => BindingForms.evalLetrec(elems.tail, env)
            case SchemeVal.Symbol("letrec*")       => BindingForms.evalLetrecStar(elems.tail, env)
            case SchemeVal.Symbol("case")          => BindingForms.evalCase(elems.tail, env)
            case SchemeVal.Symbol("do")            => BindingForms.evalDo(elems.tail, env)
            case SchemeVal.Symbol("let*")          => BindingForms.evalLetStar(elems.tail, env)
            case SchemeVal.Symbol("define-record-type") =>
              RecordType.evalDefineRecordType(elems.tail, env)
            case SchemeVal.Symbol(name) =>
              env.lookup(name) match
                case Some(m: SchemeVal.MacroVal) =>
                  val expanded = Macro.expand(m.name, m.literals, m.rules, m.defEnv, SchemeVal.SList(elems))
                  SchemeVal.TailCall(expanded, env)
                case _ =>
                  val proc = eval(elems.head, env)
                  val args = elems.tail.map(a => eval(a, env))
                  applyProc(proc, args)
            case head =>
              val proc = eval(head, env)
              val args = elems.tail.map(a => eval(a, env))
              applyProc(proc, args)
        case SchemeVal.DottedList(_, _) => throw new EvalError(s"cannot evaluate dotted list: $expr")
        case _                          => throw new EvalError(s"cannot evaluate: $expr")
    catch
      case e: EvalError =>
        val (line, col) = expr.pos
        if line > 0 && !e.getMessage.matches(".*\\d+:\\d+.*") then throw new EvalError(s"$line:$col: ${e.getMessage}")
        else throw e

  private def evalDefine(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(name) :: value :: Nil =>
        env.define(name, eval(value, env))
        SchemeVal.Void
      case SchemeVal.SList(elems) :: body if elems.nonEmpty && body.nonEmpty =>
        elems.head match
          case SchemeVal.Symbol(name) =>
            val (params, rest) = parseParams(elems.tail)
            env.define(name, SchemeVal.LambdaProc(params, body, env, rest))
            SchemeVal.Void
          case other => throw new EvalError(s"define: expected name, got $other")
      case SchemeVal.DottedList(elems, SchemeVal.Symbol(restParam)) :: body if elems.nonEmpty && body.nonEmpty =>
        elems.head match
          case SchemeVal.Symbol(name) =>
            val params = elems.tail.map {
              case SchemeVal.Symbol(p) => p
              case other               => throw new EvalError(s"expected parameter name, got $other")
            }
            env.define(name, SchemeVal.LambdaProc(params, body, env, Some(restParam)))
            SchemeVal.Void
          case other => throw new EvalError(s"define: expected name, got $other")
      case _ => throw new EvalError("define: bad syntax")

  private def evalIf(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        if isTruthy(eval(cond, env)) then SchemeVal.TailCall(thenBranch, env)
        else SchemeVal.TailCall(elseBranch, env)
      case cond :: thenBranch :: Nil =>
        if isTruthy(eval(cond, env)) then SchemeVal.TailCall(thenBranch, env)
        else SchemeVal.Void
      case _ => throw new EvalError("if: bad syntax")

  private def parseParams(paramList: List[SchemeVal]): (List[String], Option[String]) =
    val dotIdx = paramList.indexWhere {
      case SchemeVal.Symbol(".") => true
      case _                     => false
    }
    if dotIdx >= 0 then
      val fixed = paramList.take(dotIdx).map {
        case SchemeVal.Symbol(p) => p
        case other               => throw new EvalError(s"expected parameter name, got $other")
      }
      paramList.drop(dotIdx + 1) match
        case SchemeVal.Symbol(rest) :: Nil => (fixed, Some(rest))
        case _                             => throw new EvalError("bad dot syntax in parameter list")
    else
      val params = paramList.map {
        case SchemeVal.Symbol(p) => p
        case other               => throw new EvalError(s"expected parameter name, got $other")
      }
      (params, None)

  private def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(paramList) :: body if body.nonEmpty =>
        val (params, rest) = parseParams(paramList)
        SchemeVal.LambdaProc(params, body, env, rest)
      case SchemeVal.DottedList(paramList, SchemeVal.Symbol(restParam)) :: body if body.nonEmpty =>
        val params = paramList.map {
          case SchemeVal.Symbol(p) => p
          case other               => throw new EvalError(s"expected parameter name, got $other")
        }
        SchemeVal.LambdaProc(params, body, env, Some(restParam))
      case SchemeVal.Symbol(restParam) :: body if body.nonEmpty =>
        SchemeVal.LambdaProc(Nil, body, env, Some(restParam))
      case _ => throw new EvalError("lambda: bad syntax")

  @tailrec
  private def evalAnd(exprs: List[SchemeVal], env: Env): SchemeVal =
    exprs match
      case Nil         => SchemeVal.BoolVal(true)
      case last :: Nil => SchemeVal.TailCall(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if !isTruthy(result) then result else evalAnd(tail, env)

  @tailrec
  private def evalOr(exprs: List[SchemeVal], env: Env): SchemeVal =
    exprs match
      case Nil         => SchemeVal.BoolVal(false)
      case last :: Nil => SchemeVal.TailCall(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if isTruthy(result) then result else evalOr(tail, env)

  private def evalBegin(exprs: List[SchemeVal], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVal.Void
    else
      exprs.init.foreach(e => eval(e, env))
      SchemeVal.TailCall(exprs.last, env)

  private def evalDefineSyntax(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(name) :: SchemeVal.SList(srElems) :: Nil =>
        srElems.head match
          case SchemeVal.Symbol("syntax-rules") =>
            val literals = srElems(1) match
              case SchemeVal.SList(lits) =>
                lits.map {
                  case SchemeVal.Symbol(s) => s
                  case other               => throw new EvalError(s"syntax-rules: expected literal, got $other")
                }
              case _ => throw new EvalError("syntax-rules: expected literal list")
            val rules = srElems.drop(2).map {
              case SchemeVal.SList(List(pattern, template)) => (pattern, template)
              case _ => throw new EvalError("syntax-rules: expected (pattern template) clause")
            }
            env.define(name, SchemeVal.MacroVal(name, literals, rules, env))
            SchemeVal.Void
          case _ => throw new EvalError("define-syntax: expected syntax-rules")
      case _ => throw new EvalError("define-syntax: bad syntax")

  private def evalCaseLambda(clauses: List[SchemeVal], env: Env): SchemeVal =
    val parsed = clauses.map {
      case SchemeVal.SList(elems) if elems.nonEmpty =>
        elems.head match
          case SchemeVal.SList(paramList) =>
            val (params, rest) = parseParams(paramList)
            (params, rest, elems.tail)
          case SchemeVal.DottedList(paramList, SchemeVal.Symbol(restParam)) =>
            val params = paramList.map {
              case SchemeVal.Symbol(p) => p
              case other               => throw new EvalError(s"expected parameter name, got $other")
            }
            (params, Some(restParam), elems.tail)
          case SchemeVal.Symbol(restParam) =>
            (Nil, Some(restParam), elems.tail)
          case other => throw new EvalError(s"case-lambda: bad clause")
      case _ => throw new EvalError("case-lambda: bad clause")
    }
    SchemeVal.CaseLambdaProc(parsed, env)

  private def evalSet(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(name) :: value :: Nil =>
        env.set(name, eval(value, env))
        SchemeVal.Void
      case _ => throw new EvalError("set!: bad syntax")

  def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  /** Apply a procedure, returning TailCall for lambda bodies (TCO). */
  def applyProc(proc: SchemeVal, args: List[SchemeVal]): SchemeVal =
    Apply(proc, args)

  def apply(proc: SchemeVal, args: List[SchemeVal]): SchemeVal =
    val result = applyProc(proc, args)
    if result.isInstanceOf[SchemeVal.TailCall] then
      val tc = result.asInstanceOf[SchemeVal.TailCall]
      eval(tc.expr, tc.env)
    else result
