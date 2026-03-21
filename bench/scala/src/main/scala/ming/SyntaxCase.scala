package ming

/** syntax-case macro system: pattern matching, template expansion, hygiene. */
object SyntaxCase:

  private case class MatchResult(
    bindings: Map[String, Value],
    ellipsis: Map[String, List[Value]]
  )

  // --- Thread-local context for pattern variable tracking ---
  private val patVarNames = new ThreadLocal[Set[String]]:
    override def initialValue(): Set[String] = Set.empty

  private val ellVarNames = new ThreadLocal[Set[String]]:
    override def initialValue(): Set[String] = Set.empty

  private val macroDefEnv = new ThreadLocal[Option[Env]]:
    override def initialValue(): Option[Env] = None

  private val capturedFree = new ThreadLocal[Map[String, Value]]:
    override def initialValue(): Map[String, Value] = Map.empty

  private def withTransformerCtx[T](defEnv: Env)(body: => T): T =
    val sp = patVarNames.get()
    val se = ellVarNames.get()
    val sd = macroDefEnv.get()
    val sf = capturedFree.get()
    patVarNames.set(Set.empty)
    ellVarNames.set(Set.empty)
    macroDefEnv.set(Some(defEnv))
    capturedFree.set(Map.empty)
    try body
    finally
      patVarNames.set(sp)
      ellVarNames.set(se)
      macroDefEnv.set(sd)
      capturedFree.set(sf)

  // --- Transformer macro application ---
  def applyTransformer(
    mac: Value.TransformerMacro,
    form: List[Value],
    useEnv: Env
  ): (Value, Env) =
    val formValue = Value.SList(form)
    val (expanded, free) = withTransformerCtx(mac.defEnv) {
      val (result, _) = Eval.resolveBounce(
        EvalApply.applyFnBounce(mac.transformer, List(formValue))
      )
      (result, capturedFree.get())
    }
    val localEnv = free.foldLeft(useEnv) { case (e, (n, v)) => e.extend(n, v) }
    (expanded, localEnv)

  // --- syntax-case special form ---
  def evalSyntaxCaseBounce(args: List[Value], env: Env): Eval.Bounce =
    args match
      case scrutExpr :: Value.SList(lits) :: clauses =>
        val (scrutinee, out) = Eval.eval(scrutExpr, env)
        val literals = lits.map {
          case Value.Symbol(s) => s
          case other           => throw new EvalError(s"syntax-case literal must be symbol: ${other.display}")
        }.toSet
        evalClauses(scrutinee, literals, clauses, env, out)
      case _ => throw new EvalError("bad syntax-case syntax")

  private def evalClauses(
    scrut: Value,
    lits: Set[String],
    clauses: List[Value],
    env: Env,
    out: String
  ): Eval.Bounce =
    clauses match
      case Nil => throw new EvalError("syntax-case: no matching clause")
      case Value.SList(elems) :: rest =>
        elems match
          case pat :: tmpl :: Nil =>
            tryClause(pat, None, tmpl, scrut, lits, rest, env, out)
          case pat :: fender :: tmpl :: Nil =>
            tryClause(pat, Some(fender), tmpl, scrut, lits, rest, env, out)
          case _ => throw new EvalError("bad syntax-case clause")
      case other :: _ =>
        throw new EvalError(s"bad syntax-case clause: ${other.display}")

  private def tryClause(
    pat: Value,
    fender: Option[Value],
    tmpl: Value,
    scrut: Value,
    lits: Set[String],
    rest: List[Value],
    env: Env,
    out: String
  ): Eval.Bounce =
    matchPat(pat, scrut, lits) match
      case None => evalClauses(scrut, lits, rest, env, out)
      case Some(result) =>
        val newEnv = bindResult(result, env)
        fender match
          case Some(f) =>
            val (fv, fo) = Eval.eval(f, newEnv)
            if Value.isFalsy(fv) then evalClauses(scrut, lits, rest, env, out + fo)
            else
              registerPatVars(result)
              Eval.Bounce.More(tmpl, newEnv, out + fo)
          case None =>
            registerPatVars(result)
            Eval.Bounce.More(tmpl, newEnv, out)

  private def registerPatVars(result: MatchResult): Unit =
    patVarNames.set(patVarNames.get() ++ result.bindings.keySet)
    ellVarNames.set(ellVarNames.get() ++ result.ellipsis.keySet)

  private def bindResult(result: MatchResult, env: Env): Env =
    val e1 = result.bindings.foldLeft(env) { case (e, (n, v)) => e.extend(n, v) }
    result.ellipsis.foldLeft(e1) { case (e, (n, vs)) => e.extend(n, Value.SList(vs)) }

  // --- Pattern matching ---
  private def matchPat(pat: Value, input: Value, lits: Set[String]): Option[MatchResult] =
    pat match
      case Value.Symbol("_") => Some(MatchResult(Map.empty, Map.empty))
      case Value.Symbol(n) if !lits.contains(n) && n != "..." =>
        Some(MatchResult(Map(n -> input), Map.empty))
      case Value.Symbol(n) if lits.contains(n) =>
        input match
          case Value.Symbol(m) if m == n => Some(MatchResult(Map.empty, Map.empty))
          case _                         => None
      case Value.SList(pats) =>
        input match
          case Value.SList(ins) => matchList(pats, ins, lits)
          case _                => None
      case _ =>
        if pat == input then Some(MatchResult(Map.empty, Map.empty)) else None

  private def matchList(
    pats: List[Value],
    ins: List[Value],
    lits: Set[String]
  ): Option[MatchResult] =
    pats match
      case Nil =>
        if ins.isEmpty then Some(MatchResult(Map.empty, Map.empty)) else None
      case p :: Value.Symbol("...") :: Nil =>
        p match
          case Value.Symbol(n) if n != "_" && !lits.contains(n) =>
            Some(MatchResult(Map.empty, Map(n -> ins)))
          case _ => None
      case p :: rest =>
        ins match
          case Nil => None
          case i :: ri =>
            for
              r1 <- matchPat(p, i, lits)
              r2 <- matchList(rest, ri, lits)
            yield MatchResult(r1.bindings ++ r2.bindings, r1.ellipsis ++ r2.ellipsis)

  // --- syntax (aka #') template expansion ---
  def evalSyntaxBounce(args: List[Value], env: Env): Eval.Bounce =
    args match
      case tmpl :: Nil =>
        val pats     = patVarNames.get()
        val ells     = ellVarNames.get()
        val expanded = expandTmpl(tmpl, env, pats, ells)
        captureFreeBindings(tmpl, pats, ells)
        Eval.Bounce.Done(expanded, "")
      case _ => throw new EvalError("syntax requires exactly 1 argument")

  private def captureFreeBindings(
    tmpl: Value,
    pats: Set[String],
    ells: Set[String]
  ): Unit =
    macroDefEnv.get().foreach { defEnv =>
      val syms = collectSymbols(tmpl) -- pats -- ells -- specialForms - "..."
      val bindings = syms.foldLeft(Map.empty[String, Value]) { (acc, name) =>
        try
          defEnv.lookup(name) match
            case _: Value.Macro | _: Value.TransformerMacro => acc
            case v                                          => acc + (name -> v)
        catch case _: EvalError => acc
      }
      capturedFree.set(capturedFree.get() ++ bindings)
    }

  private def expandTmpl(
    tmpl: Value,
    env: Env,
    pats: Set[String],
    ells: Set[String]
  ): Value =
    tmpl match
      case Value.Symbol(n) if pats.contains(n) || ells.contains(n) =>
        env.lookup(n)
      case Value.SList(elems) =>
        Value.SList(expandListTmpl(elems, env, pats, ells))
      case other => other

  private def expandListTmpl(
    elems: List[Value],
    env: Env,
    pats: Set[String],
    ells: Set[String]
  ): List[Value] =
    elems match
      case Nil => Nil
      case p :: Value.Symbol("...") :: rest =>
        findEllVar(p, ells) match
          case Some(name) =>
            val values = env.lookup(name) match
              case Value.SList(vs) => vs
              case other           => List(other)
            val exp = values.map { v =>
              expandTmpl(p, env.extend(name, v), pats + name, ells - name)
            }
            exp ++ expandListTmpl(rest, env, pats, ells)
          case None =>
            expandTmpl(p, env, pats, ells) :: expandListTmpl(rest, env, pats, ells)
      case h :: rest =>
        expandTmpl(h, env, pats, ells) :: expandListTmpl(rest, env, pats, ells)

  private def findEllVar(tmpl: Value, ells: Set[String]): Option[String] =
    tmpl match
      case Value.Symbol(n) if ells.contains(n) => Some(n)
      case Value.SList(es) =>
        es.collectFirst { e =>
          findEllVar(e, ells) match
            case Some(n) => n
        }
      case _ => None

  // --- with-syntax ---
  def evalWithSyntaxBounce(args: List[Value], env: Env): Eval.Bounce =
    args match
      case Value.SList(bindings) :: body if body.nonEmpty =>
        val (newEnv, out) = processBindings(bindings, env)
        Eval.evalBodyBounce(body, newEnv, out)
      case _ => throw new EvalError("bad with-syntax syntax")

  private def processBindings(bindings: List[Value], env: Env): (Env, String) =
    bindings.foldLeft((env, "")) { case ((e, out), b) =>
      b match
        case Value.SList(Value.Symbol(name) :: expr :: Nil) =>
          val (value, o) = Eval.eval(expr, e)
          patVarNames.set(patVarNames.get() + name)
          (e.extend(name, value), out + o)
        case _ => throw new EvalError("bad with-syntax binding")
    }

  private def collectSymbols(v: Value): Set[String] = v match
    case Value.Symbol(n) => Set(n)
    case Value.SList(es) => es.flatMap(collectSymbols).toSet
    case _               => Set.empty

  private val specialForms: Set[String] = Set(
    "quote",
    "if",
    "lambda",
    "define",
    "set!",
    "let",
    "begin",
    "cond",
    "and",
    "or",
    "define-syntax",
    "syntax-rules",
    "letrec",
    "letrec*",
    "case",
    "syntax-case",
    "syntax",
    "with-syntax"
  )
