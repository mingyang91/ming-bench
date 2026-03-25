package ming

object Evaluator:

  import Builtins.{applyBuiltin, isFalsy}

  private def display(e: Expr): String = Display.display(e)

  private[ming] def eval(expr: Expr, env: Env): Expr =
    try
      expr match
        case Expr.Num(_) | Expr.Rational(_, _) | Expr.Real(_) | Expr.Bool(_) | Expr.Str(_, _) | Expr.Chr(_) |
            Expr.Lambda(_, _, _, _) | Expr.Pair(_, _) | Expr.Macro(_, _, _) | Expr.Record(_, _, _, _) |
            Expr.CaseLambda(_, _) | Expr.Vec(_) =>
          expr
        case Expr.Sym(name) => env.lookup(name)
        case Expr.Lst(Nil)  => throw EvalError("empty application")
        case Expr.Lst(Expr.Sym("quote") :: args) =>
          if args.length != 1 then throw EvalError("quote: need exactly 1 argument")
          args.head
        case Expr.Lst(Expr.Sym("if") :: args)                 => evalIf(args, env)
        case Expr.Lst(Expr.Sym("define") :: args)             => evalDefine(args, env)
        case Expr.Lst(Expr.Sym("set!") :: args)               => evalSet(args, env)
        case Expr.Lst(Expr.Sym("lambda") :: args)             => evalLambda(args, env)
        case Expr.Lst(Expr.Sym("let") :: args)                => evalLet(args, env)
        case Expr.Lst(Expr.Sym("begin") :: args)              => evalBegin(args, env)
        case Expr.Lst(Expr.Sym("cond") :: clauses)            => evalCond(clauses, env)
        case Expr.Lst(Expr.Sym("and") :: args)                => evalAnd(args, env)
        case Expr.Lst(Expr.Sym("or") :: args)                 => evalOr(args, env)
        case Expr.Lst(Expr.Sym("define-syntax") :: args)      => evalDefineSyntax(args, env)
        case Expr.Lst(Expr.Sym("define-record-type") :: args) => RecordOps.evalDefineRecordType(args, env)
        case Expr.Lst(Expr.Sym("case-lambda") :: clauses)     => evalCaseLambda(clauses, env)
        case Expr.Lst(Expr.Sym("letrec") :: args)             => evalLetrec(args, env)
        case Expr.Lst(Expr.Sym("letrec*") :: args)            => evalLetrecStar(args, env)
        case Expr.Lst(Expr.Sym("case") :: args)               => evalCase(args, env)
        case Expr.Lst(Expr.Sym("do") :: args)                 => evalDo(args, env)
        case Expr.Lst((head @ Expr.Sym(name)) :: _) if isMacro(name, env) =>
          val mac = env.lookup(name).asInstanceOf[Expr.Macro]
          val (expanded, hygieneEnv) =
            Macros.expandMacro(mac.literals, mac.rules, expr.asInstanceOf[Expr.Lst].elems, mac.defEnv, env)
          eval(expanded, hygieneEnv)
        case Expr.Lst(op :: args) =>
          val func          = eval(op, env)
          val evaluatedArgs = args.map(a => eval(a, env))
          applyProc(func, evaluatedArgs)
    catch
      case e: EvalError if expr.line > 0 && !e.getMessage.matches(".*\\d+:\\d+.*") =>
        throw EvalError(s"${expr.line}:${expr.col}: ${e.getMessage}")

  private def evalIf(args: List[Expr], env: Env): Expr =
    if args.length < 2 || args.length > 3 then throw EvalError("if: need 2 or 3 arguments")
    val cond = eval(args.head, env)
    if !isFalsy(cond) then eval(args(1), env)
    else if args.length == 3 then eval(args(2), env)
    else Expr.Bool(false)

  private def evalDefine(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.define(name, eval(value, env))
      Expr.Bool(false)
    case Expr.Lst(Expr.Sym(name) :: params) :: body if body.nonEmpty =>
      val (paramNames, restParam) = ParamUtils.extractParamsWithRest("define", params)
      env.define(name, Expr.Lambda(paramNames, restParam, body, env))
      Expr.Bool(false)
    case _ => throw EvalError("define: invalid syntax")

  private def evalSet(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.set(name, eval(value, env))
      Expr.Bool(false)
    case _ => throw EvalError("set!: invalid syntax")

  private def evalLambda(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(params) :: body if body.nonEmpty =>
      val (paramNames, restParam) = ParamUtils.extractParamsWithRest("lambda", params)
      Expr.Lambda(paramNames, restParam, body, env)
    case Expr.Sym(restName) :: body if body.nonEmpty =>
      Expr.Lambda(Nil, Some(restName), body, env)
    case _ => throw EvalError("lambda: invalid syntax")

  private def evalLet(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv     = env.child()
      val paramNames = List.newBuilder[String]
      val initVals   = List.newBuilder[Expr]
      for b <- bindings do
        b match
          case Expr.Lst(List(Expr.Sym(p), valueExpr)) =>
            paramNames += p
            initVals += eval(valueExpr, env)
          case _ => throw EvalError("let: invalid binding")
      val lambda = Expr.Lambda(paramNames.result(), None, body, letEnv)
      letEnv.define(name, lambda)
      applyProc(lambda, initVals.result())
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      for b <- bindings do
        b match
          case Expr.Lst(List(Expr.Sym(name), valueExpr)) =>
            letEnv.define(name, eval(valueExpr, env))
          case _ => throw EvalError("let: invalid binding")
      evalBody(body, letEnv)
    case _ => throw EvalError("let: invalid syntax")

  private def evalBegin(args: List[Expr], env: Env): Expr =
    if args.isEmpty then throw EvalError("begin: need at least 1 expression")
    evalBody(args, env)

  private def evalAnd(args: List[Expr], env: Env): Expr = args match
    case Nil         => Expr.Bool(true)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if isFalsy(v) then v else evalAnd(tail, env)

  private def evalOr(args: List[Expr], env: Env): Expr = args match
    case Nil         => Expr.Bool(false)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if !isFalsy(v) then v else evalOr(tail, env)

  private def evalCond(clauses: List[Expr], env: Env): Expr = clauses match
    case Nil                                     => Expr.Bool(false)
    case Expr.Lst(Expr.Sym("else") :: body) :: _ => evalBody(body, env)
    case Expr.Lst(test :: body) :: rest =>
      val v = eval(test, env)
      if !isFalsy(v) then if body.isEmpty then v else evalBody(body, env)
      else evalCond(rest, env)
    case _ => throw EvalError("cond: invalid clause")

  private def isMacro(name: String, env: Env): Boolean =
    env.lookupOpt(name).exists(_.isInstanceOf[Expr.Macro])

  private def evalDefineSyntax(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: Expr.Lst(Expr.Sym("syntax-rules") :: Expr.Lst(literals) :: rules) :: Nil =>
      val litNames = literals.map {
        case Expr.Sym(s) => s
        case _           => throw EvalError("define-syntax: literals must be symbols")
      }
      val rulesList = rules.map {
        case Expr.Lst(List(Expr.Lst(_ :: patternArgs), template)) =>
          (patternArgs, template)
        case _ => throw EvalError("define-syntax: invalid rule")
      }
      env.define(name, Expr.Macro(litNames, rulesList, env))
      Expr.Bool(false)
    case _ => throw EvalError("define-syntax: invalid syntax")

  private def evalCaseLambda(clauses: List[Expr], env: Env): Expr =
    val parsed = clauses.map {
      case Expr.Lst(Expr.Lst(params) :: body) if body.nonEmpty =>
        val (paramNames, restParam) = ParamUtils.extractParamsWithRest("case-lambda", params)
        (paramNames, restParam, body)
      case _ => throw EvalError("case-lambda: invalid clause")
    }
    Expr.CaseLambda(parsed, env)

  private def evalLetrec(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      // First define all variables as uninitialized (use #f placeholder)
      val names = bindings.map {
        case Expr.Lst(List(Expr.Sym(name), _)) => name
        case _                                 => throw EvalError("letrec: invalid binding")
      }
      for name <- names do letEnv.define(name, Expr.Bool(false))
      // Now evaluate init expressions in the letrec environment and assign
      for b <- bindings do
        b match
          case Expr.Lst(List(Expr.Sym(name), valueExpr)) =>
            letEnv.define(name, eval(valueExpr, letEnv))
          case _ => throw EvalError("letrec: invalid binding")
      evalBody(body, letEnv)
    case _ => throw EvalError("letrec: invalid syntax")

  private def evalLetrecStar(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      for b <- bindings do
        b match
          case Expr.Lst(List(Expr.Sym(name), valueExpr)) =>
            letEnv.define(name, eval(valueExpr, letEnv))
          case _ => throw EvalError("letrec*: invalid binding")
      evalBody(body, letEnv)
    case _ => throw EvalError("letrec*: invalid syntax")

  private def evalCase(args: List[Expr], env: Env): Expr =
    if args.isEmpty then throw EvalError("case: need key expression")
    val key = eval(args.head, env)
    evalCaseClauses(key, args.tail, env)

  private def evalCaseClauses(key: Expr, clauses: List[Expr], env: Env): Expr = clauses match
    case Nil                                     => Expr.Bool(false) // unspecified when no match
    case Expr.Lst(Expr.Sym("else") :: body) :: _ => evalBody(body, env)
    case Expr.Lst(Expr.Lst(datums) :: body) :: rest =>
      if datums.exists(d => EqualityOps.eqv(key, d)) then evalBody(body, env)
      else evalCaseClauses(key, rest, env)
    case _ => throw EvalError("case: invalid clause")

  private def evalDo(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(varSpecs) :: Expr.Lst(testAndExprs) :: body =>
      if testAndExprs.isEmpty then throw EvalError("do: need test expression")
      val test        = testAndExprs.head
      val resultExprs = testAndExprs.tail
      // Parse variable specs: (var init step?)
      val vars = varSpecs.map {
        case Expr.Lst(Expr.Sym(name) :: init :: step :: Nil) => (name, init, Some(step))
        case Expr.Lst(Expr.Sym(name) :: init :: Nil)         => (name, init, None)
        case _                                               => throw EvalError("do: invalid variable spec")
      }
      val doEnv = env.child()
      // Initialize variables
      for (name, init, _) <- vars do doEnv.define(name, eval(init, env))
      // Iteration loop
      while true do
        val testResult = eval(test, doEnv)
        if !isFalsy(testResult) then
          // Test passed — evaluate result expressions
          if resultExprs.isEmpty then return testResult
          else return evalBody(resultExprs, doEnv)
        // Execute body
        for expr <- body do eval(expr, doEnv)
        // Parallel step: evaluate all steps using current values, then update
        val newVals = vars.map {
          case (_, _, Some(step)) => Some(eval(step, doEnv))
          case (_, _, None)       => None
        }
        for ((name, _, _), newVal) <- vars.zip(newVals) do newVal.foreach(v => doEnv.define(name, v))
      Expr.Bool(false) // unreachable
    case _ => throw EvalError("do: invalid syntax")

  private[ming] def evalBody(exprs: List[Expr], env: Env): Expr =
    exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => eval(e, env))

  private def applyProc(func: Expr, args: List[Expr]): Expr = func match
    case Expr.Sym(name) if name == "apply"              => applyApply(args)
    case Expr.Sym(name) if name == "map"                => applyMap(args)
    case Expr.Sym(name) if name.startsWith("%%record-") => RecordOps.applyRecordOp(name, args)
    case Expr.Sym(name)                                 => applyBuiltin(name, args)
    case Expr.Lambda(params, restParam, body, closure) =>
      val localEnv = closure.child()
      params.zip(args).foreach((p, a) => localEnv.define(p, a))
      restParam match
        case None =>
          if params.length != args.length then
            throw EvalError(s"lambda: expected ${params.length} arguments, got ${args.length}")
          evalBody(body, localEnv)
        case Some(rest) =>
          if args.length < params.length then
            throw EvalError(s"lambda: expected at least ${params.length} arguments, got ${args.length}")
          localEnv.define(rest, Expr.Lst(args.drop(params.length)))
          evalBody(body, localEnv)
    case Expr.CaseLambda(clauses, closure) =>
      val matched = clauses.find { case (params, restParam, _) =>
        restParam match
          case None    => args.length == params.length
          case Some(_) => args.length >= params.length
      }
      matched match
        case Some((params, restParam, body)) =>
          val localEnv = closure.child()
          params.zip(args).foreach((p, a) => localEnv.define(p, a))
          restParam.foreach(rest => localEnv.define(rest, Expr.Lst(args.drop(params.length))))
          evalBody(body, localEnv)
        case None =>
          throw EvalError(s"case-lambda: no matching clause for ${args.length} arguments")
    case _ => throw EvalError(s"not a procedure: ${display(func)}")

  private def applyMap(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("map: need at least 2 arguments")
    val proc = args.head
    val lists = args.tail.map {
      case Expr.Lst(elems) => elems
      case other           => throw EvalError(s"map: not a list: ${display(other)}")
    }
    val len = lists.head.length
    if !lists.forall(_.length == len) then throw EvalError("map: lists must have equal length")
    val result = (0 until len).toList.map { i =>
      val argSlice = lists.map(_(i))
      applyProc(proc, argSlice)
    }
    Expr.Lst(result)

  private def applyApply(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("apply: need at least 2 arguments")
    val func = args.head
    val lastArg = args.last match
      case Expr.Lst(elems) => elems
      case other           => throw EvalError(s"apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    applyProc(func, prefixArgs ++ lastArg)
