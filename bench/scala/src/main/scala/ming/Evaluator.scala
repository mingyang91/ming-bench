package ming

/** Scheme interpreter — CEK machine with first-class continuations. */
object Evaluator:

  /** Thread-local dynamic-wind stack (head = innermost extent). */
  private[ming] val windStack: ThreadLocal[List[WindEntry]] =
    ThreadLocal.withInitial(() => Nil)

  /** Thread-local exception handler stack (head = innermost handler). */
  private[ming] val handlerStack: ThreadLocal[List[ExceptionHandler]] =
    ThreadLocal.withInitial(() => Nil)

  private[ming] enum State:
    case Ev(expr: SchemeVal, env: Env, k: Cont)
    case Ko(value: SchemeVal, k: Cont)

  private[ming] def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private[ming] def evalBody(body: List[SchemeVal], env: Env): SchemeVal =
    body match
      case Nil           => SchemeVal.SVoid
      case last :: Nil   => run(State.Ev(last, env, Cont.Halt))
      case first :: rest => run(State.Ev(first, env, Cont.SeqK(rest, env, Cont.Halt)))

  def eval(initExpr: SchemeVal, initEnv: Env): SchemeVal =
    run(State.Ev(initExpr, initEnv, Cont.Halt))

  /** Thread-local step limit: None = unlimited, Some(n) = n steps remaining. */
  private[ming] val stepLimit: ThreadLocal[Option[Int]] =
    ThreadLocal.withInitial(() => None)

  private def run(initState: State): SchemeVal =
    var state                = initState
    var lastPos: Option[Pos] = None
    while true do
      state match
        case State.Ko(v, Cont.Halt) => return v
        case _                      =>
          // Check step limit
          stepLimit.get() match
            case Some(n) if n <= 0 =>
              throw new EvalError("step limit exceeded")
            case Some(n) =>
              stepLimit.set(Some(n - 1))
            case None => ()
          // Track position from the most recent compound expression
          state match
            case State.Ev(expr @ SchemeVal.SList(_), _, _) if expr.pos.isDefined =>
              lastPos = expr.pos
            case _ => ()
          try state = step(state)
          catch
            case e: EvalError if !e.hasPosition =>
              lastPos match
                case Some(p) =>
                  throw new EvalError(s"${e.getMessage} at ${p.line}:${p.col}", hasPosition = true)
                case None => throw e
    throw new RuntimeException("unreachable")

  private def step(s: State): State = s match
    case State.Ev(expr, env, k) => evalStep(expr, env, k)
    case State.Ko(v, k)         => kontStep(v, k)

  private def evalStep(expr: SchemeVal, env: Env, k: Cont): State =
    expr match
      case SchemeVal.SInt(_) | SchemeVal.SFloat(_) | SchemeVal.SRational(_, _) | SchemeVal.SBool(_) |
          SchemeVal.SString(_, _) | SchemeVal.SChar(_) | SchemeVal.SVoid | SchemeVal.SPair(_) | SchemeVal.SVector(_) =>
        State.Ko(expr, k)
      case SchemeVal.SSymbol(name) => State.Ko(env.get(name), k)
      case SchemeVal.SList(elems)  => dispatchList(elems, env, k)
      case other                   => State.Ko(other, k)

  private def dispatchList(elems: List[SchemeVal], env: Env, k: Cont): State =
    elems match
      case Nil => throw new EvalError("empty application")
      case SchemeVal.SSymbol("quote") :: args =>
        if args.length != 1 then throw new EvalError("quote: expected 1 argument")
        State.Ko(args.head, k)
      case SchemeVal.SSymbol("quasiquote") :: args =>
        if args.length != 1 then throw new EvalError("quasiquote: expected 1 argument")
        State.Ko(expandQuasiquote(args.head, env), k)
      case SchemeVal.SSymbol("if") :: args =>
        if args.length < 2 || args.length > 3 then throw new EvalError("if: expected 2 or 3 arguments")
        State.Ev(args(0), env, Cont.IfK(args(1), args.lift(2), env, k))
      case SchemeVal.SSymbol("define") :: args =>
        LetForms.evalDefineStep(args, env, k)
      case SchemeVal.SSymbol("lambda") :: args =>
        State.Ko(DefineForms.evalLambda(args, env), k)
      case SchemeVal.SSymbol("and") :: args =>
        if args.isEmpty then State.Ko(SchemeVal.SBool(true), k)
        else if args.length == 1 then State.Ev(args.head, env, k)
        else State.Ev(args.head, env, Cont.AndK(args.tail, env, k))
      case SchemeVal.SSymbol("or") :: args =>
        if args.isEmpty then State.Ko(SchemeVal.SBool(false), k)
        else if args.length == 1 then State.Ev(args.head, env, k)
        else State.Ev(args.head, env, Cont.OrK(args.tail, env, k))
      case SchemeVal.SSymbol("let") :: args =>
        LetForms.evalLetStep(args, env, k)
      case SchemeVal.SSymbol("set!") :: args =>
        args match
          case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
            State.Ev(valueExpr, env, Cont.SetValK(name, env, k))
          case _ => throw new EvalError("set!: bad syntax")
      case SchemeVal.SSymbol("begin") :: args =>
        evalBodyCek(args, env, k)
      case SchemeVal.SSymbol("cond") :: clauses =>
        LetForms.evalCondStep(clauses, env, k)
      case SchemeVal.SSymbol("define-syntax") :: args =>
        State.Ko(DefineForms.evalDefineSyntax(args, env), k)
      case SchemeVal.SSymbol("case-lambda") :: args =>
        State.Ko(BindingForms.evalCaseLambda(args, env), k)
      case SchemeVal.SSymbol("define-record-type") :: args =>
        State.Ko(RecordOps.evalDefineRecordType(args, env), k)
      case SchemeVal.SSymbol("letrec") :: args =>
        LetForms.evalLetrecStep(args, env, k)
      case SchemeVal.SSymbol("letrec*") :: args =>
        LetForms.evalLetrecStarStep(args, env, k)
      case SchemeVal.SSymbol("case") :: args =>
        State.Ko(BindingForms.evalCase(args, env), k)
      case SchemeVal.SSymbol("do") :: args =>
        State.Ko(BindingForms.evalDo(args, env), k)
      case SchemeVal.SSymbol("let*") :: args =>
        LetForms.evalLetStarStep(args, env, k)
      case SchemeVal.SSymbol("syntax-case") :: args =>
        args match
          case stxExpr :: SchemeVal.SList(literals) :: clauses if clauses.nonEmpty =>
            val stxVal   = eval(stxExpr, env)
            val litNames = literals.collect { case SchemeVal.SSymbol(n) => n }.toSet
            State.Ko(SyntaxCase.evalSyntaxCase(stxVal, litNames, clauses, env), k)
          case _ => throw new EvalError("syntax-case: bad syntax")
      case SchemeVal.SSymbol("syntax") :: args =>
        args match
          case template :: Nil => State.Ko(SyntaxCase.evalSyntax(template), k)
          case _               => throw new EvalError("syntax: expected 1 argument")
      case SchemeVal.SSymbol("with-syntax") :: args =>
        args match
          case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
            State.Ko(SyntaxCase.evalWithSyntax(bindings, body, env), k)
          case _ => throw new EvalError("with-syntax: bad syntax")
      case SchemeVal.SSymbol("guard") :: args =>
        args match
          case SchemeVal.SList(SchemeVal.SSymbol(exnVar) :: clauses) :: body if body.nonEmpty =>
            val handler = ExceptionHandler.Guard(exnVar, clauses, env, k, windStack.get())
            handlerStack.set(handler :: handlerStack.get())
            evalBodyCek(body, env, Cont.WithHandlerK(k))
          case _ => throw new EvalError("guard: bad syntax")
      case SchemeVal.SSymbol(name) :: _ if env.lookup(name).exists(isMacro) =>
        env.get(name) match
          case macro_ : SchemeVal.SMacro =>
            val expanded = Macro.expand(macro_, SchemeVal.SList(elems))
            State.Ev(expanded, env, k)
          case SchemeVal.STransformerMacro(transformer, defEnv, defBound) =>
            val form  = SchemeVal.SList(elems)
            val stack = SyntaxCase.contextStack.get()
            val ctx   = SyntaxCase.Context(Map.empty, defEnv, defBound)
            SyntaxCase.contextStack.set(ctx :: stack)
            val expanded =
              try
                transformer match
                  case SchemeVal.SLambda(params, rest, body, closure) =>
                    val callEnv = Apply.setupCallEnv(params, rest, List(form), closure)
                    evalBody(body, callEnv)
                  case SchemeVal.SCaseLambda(clauses, closure) =>
                    val (p, r, b) = Apply.findClause(clauses, List(form))
                    val callEnv   = Apply.setupCallEnv(p, r, List(form), closure)
                    evalBody(b, callEnv)
                  case _ => throw new EvalError("transformer macro: not a procedure")
              finally SyntaxCase.contextStack.set(stack)
            State.Ev(expanded, env, k)
      case head :: args =>
        State.Ev(head, env, Cont.EvOpK(args, env, k))

  private[ming] def evalBodyCek(body: List[SchemeVal], env: Env, k: Cont): State =
    body match
      case Nil           => State.Ko(SchemeVal.SVoid, k)
      case last :: Nil   => State.Ev(last, env, k)
      case first :: rest => State.Ev(first, env, Cont.SeqK(rest, env, k))

  private def kontStep(v: SchemeVal, k: Cont): State = k match
    case Cont.Halt => State.Ko(v, k)
    case Cont.IfK(thenE, elseE, env, k2) =>
      if isTruthy(v) then State.Ev(thenE, env, k2)
      else
        elseE match
          case Some(e) => State.Ev(e, env, k2)
          case None    => State.Ko(SchemeVal.SVoid, k2)
    case Cont.SeqK(rest, env, k2) =>
      rest match
        case last :: Nil  => State.Ev(last, env, k2)
        case next :: more => State.Ev(next, env, Cont.SeqK(more, env, k2))
        case Nil          => State.Ko(SchemeVal.SVoid, k2)
    case Cont.DefValK(name, env, k2) =>
      env.define(name, v)
      State.Ko(SchemeVal.SVoid, k2)
    case Cont.SetValK(name, env, k2) =>
      env.set(name, v)
      State.Ko(SchemeVal.SVoid, k2)
    case Cont.EvOpK(argExprs, env, k2) =>
      if argExprs.isEmpty then Apply.performApply(v, Nil, k2)
      else
        // Right-to-left argument evaluation (matches Chez Scheme)
        val rev = argExprs.reverse
        State.Ev(rev.head, env, Cont.EvArgK(v, Nil, rev.tail, env, k2))
    case Cont.EvArgK(op, done, rest, env, k2) =>
      val newDone = v :: done // prepend; with reversed eval order, this builds correct order
      if rest.isEmpty then Apply.performApply(op, newDone, k2)
      else State.Ev(rest.head, env, Cont.EvArgK(op, newDone, rest.tail, env, k2))
    case Cont.AndK(rest, env, k2) =>
      if !isTruthy(v) then State.Ko(v, k2)
      else
        rest match
          case last :: Nil  => State.Ev(last, env, k2)
          case next :: more => State.Ev(next, env, Cont.AndK(more, env, k2))
          case Nil          => State.Ko(v, k2)
    case Cont.OrK(rest, env, k2) =>
      if isTruthy(v) then State.Ko(v, k2)
      else
        rest match
          case last :: Nil  => State.Ev(last, env, k2)
          case next :: more => State.Ev(next, env, Cont.OrK(more, env, k2))
          case Nil          => State.Ko(v, k2)
    case Cont.CondK(body, remaining, env, k2) =>
      if isTruthy(v) then
        body match
          case SchemeVal.SSymbol("=>") :: proc :: Nil =>
            // (cond (test => proc)) — evaluate proc, then apply to test result
            State.Ev(proc, env, Cont.CondArrowK(v, env, k2))
          case _ =>
            if body.isEmpty then State.Ko(v, k2)
            else evalBodyCek(body, env, k2)
      else LetForms.evalCondStep(remaining, env, k2)
    case Cont.CondArrowK(testValue, env, k2) =>
      // proc has been evaluated to v; apply it to testValue
      Apply.performApply(v, List(testValue), k2)
    case other => KontOps.step(v, other)

  /** Expand quasiquote template, evaluating unquote and unquote-splicing. */
  private def expandQuasiquote(tmpl: SchemeVal, env: Env): SchemeVal =
    tmpl match
      case SchemeVal.SList(SchemeVal.SSymbol("unquote") :: expr :: Nil) =>
        eval(expr, env)
      case SchemeVal.SList(elems) =>
        expandQuasiquoteList(elems, env)
      case SchemeVal.SPair(cell) =>
        cell.car match
          case SchemeVal.SSymbol("unquote") =>
            // (unquote expr) as pair — eval the cdr's car
            val expr = SchemeVal.pairCar(cell.cdr)
            eval(expr, env)
          case _ =>
            val newCar = expandQuasiquote(cell.car, env)
            val newCdr = expandQuasiquote(cell.cdr, env)
            SchemeVal.SPair(new MutableCell(newCar, newCdr))
      case SchemeVal.SVector(elems) =>
        SchemeVal.SVector(elems.map(expandQuasiquote(_, env)))
      case _ => tmpl

  /** Expand quasiquote for list elements, handling unquote-splicing. */
  private def expandQuasiquoteList(elems: List[SchemeVal], env: Env): SchemeVal =
    val result          = scala.collection.mutable.ListBuffer[SchemeVal]()
    var tail: SchemeVal = SchemeVal.SList(Nil)
    var i               = 0
    while i < elems.length do
      elems(i) match
        case SchemeVal.SList(SchemeVal.SSymbol("unquote-splicing") :: expr :: Nil) =>
          val spliced = eval(expr, env)
          SchemeVal.toScalaList(spliced) match
            case Some(items) => result ++= items
            case None        => throw new EvalError(s"unquote-splicing: expected list, got ${spliced.display}")
        case SchemeVal.SList(SchemeVal.SSymbol("unquote") :: expr :: Nil) =>
          result += eval(expr, env)
        case other =>
          result += expandQuasiquote(other, env)
      i += 1
    if result.isEmpty then SchemeVal.SList(Nil)
    else SchemeVal.buildList(result.toList)

  private def isMacro(v: SchemeVal): Boolean = v match
    case _: SchemeVal.SMacro            => true
    case _: SchemeVal.STransformerMacro => true
    case _                              => false

  private def makeGlobalEnv(): Env =
    val env = Env()
    for name <- Builtins.names do env.define(name, SchemeVal.SSymbol(name))
    env

  def evalStr(input: String): String =
    windStack.set(Nil)
    handlerStack.set(Nil)
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    evalBody(exprs, env).display

  def evalStrWithLimit(input: String, limit: Int): String =
    windStack.set(Nil)
    handlerStack.set(Nil)
    stepLimit.set(Some(limit))
    try
      val exprs = Parser.parseAll(input)
      if exprs.isEmpty then throw new EvalError("empty input")
      val env = makeGlobalEnv()
      evalBody(exprs, env).display
    finally stepLimit.set(None)

  def evalStrWithOutput(input: String): (String, String) =
    windStack.set(Nil)
    handlerStack.set(Nil)
    val buf = HigherOrder.outputBuffer.get()
    buf.clear()
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env    = makeGlobalEnv()
    val result = evalBody(exprs, env).display
    val output = buf.toString
    buf.clear()
    (result, output)
