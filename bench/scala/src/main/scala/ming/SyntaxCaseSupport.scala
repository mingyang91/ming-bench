package ming

import SchemeTypes.{errAt, Env, Pos, Value}

/** Helpers for syntax-case macro expansion. */
object SyntaxCaseSupport:

  private val specialForms: Set[String] = Set(
    "quote",
    "define",
    "if",
    "lambda",
    "and",
    "or",
    "let",
    "begin",
    "cond",
    "set!",
    "define-syntax",
    "syntax-rules",
    "else",
    "syntax-case",
    "syntax",
    "with-syntax",
    "let*",
    "letrec",
    "letrec*",
    "case",
    "do",
    "guard",
    "case-lambda",
    "define-record-type",
    "dynamic-wind"
  )

  private val gensymCounter = java.util.concurrent.atomic.AtomicLong(0L)

  private def gensym(base: String): String =
    val id = gensymCounter.incrementAndGet()
    s"$base##sc$id"

  /** Evaluate a syntax-case form, returning the matched clause result. */
  def evalSyntaxCase(
    stxExpr: Expr,
    lits: List[Expr],
    clauses: List[Expr],
    env: Env,
    pos: Pos,
    evalExpr: (Expr, Env) => Value
  ): Value =
    val literals = lits.map {
      case Expr.Symbol(s, _) => s
      case _                 => throw errAt(pos, "invalid syntax-case literals")
    }
    val stxVal = evalExpr(stxExpr, env)
    val inputExpr = stxVal match
      case Value.VSyntax(e, _) => e
      case _                   => throw errAt(pos, "syntax-case: expected syntax object")
    val inputElems = inputExpr match
      case Expr.SList(elems, _) => elems
      case _                    => List(inputExpr)
    clauses.iterator
      .map {
        case Expr.SList(pat :: body, _) if body.nonEmpty =>
          val (fender, templateExpr) =
            if body.length >= 2 then (Some(body.head), body(1))
            else (None, body.head)
          val patElems = pat match
            case Expr.SList(elems, _) => elems
            case _                    => List(pat)
          MacroExpander.matchSyntaxCase(patElems, inputElems, literals).flatMap { bindings =>
            val clauseEnv = env.child()
            bindPatternVars(bindings, clauseEnv)
            fender match
              case Some(fenderExpr) =>
                val fenderVal = evalExpr(fenderExpr, clauseEnv)
                if SchemeTypes.isTruthy(fenderVal) then Some(evalExpr(templateExpr, clauseEnv))
                else None
              case None =>
                Some(evalExpr(templateExpr, clauseEnv))
          }
        case e => throw errAt(CekSteps.posOf(e), "invalid syntax-case clause")
      }
      .collectFirst { case Some(r) => r }
      .getOrElse(throw errAt(pos, "no matching syntax-case clause"))

  /** Evaluate a with-syntax form. */
  def evalWithSyntax(
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    evalExpr: (Expr, Env) => Value,
    evalBody: (List[Expr], Env) => Value
  ): Value =
    val wsEnv = env.child()
    for b <- bindings do
      b match
        case Expr.SList(pat :: expr :: Nil, bp) =>
          val v = evalExpr(expr, env)
          val syntaxExpr = v match
            case Value.VSyntax(e, _) => e
            case _                   => throw errAt(bp, "with-syntax: expected syntax object")
          val patNames = pat match
            case Expr.Symbol(name, _) =>
              Map(name -> Left(syntaxExpr)): MacroExpander.PublicBindings
            case Expr.SList(elems, _) =>
              MacroExpander
                .matchSyntaxCase(elems, List(syntaxExpr), Nil)
                .getOrElse(throw errAt(bp, "with-syntax: pattern mismatch"))
            case _ => throw errAt(bp, "invalid with-syntax pattern")
          bindPatternVars(patNames, wsEnv)
        case e => throw errAt(CekSteps.posOf(e), "invalid with-syntax binding")
    evalBody(body, wsEnv)

  /** Bind pattern variable bindings (from MacroExpander) into an environment. */
  def bindPatternVars(bindings: MacroExpander.PublicBindings, env: Env): Unit =
    for (name, binding) <- bindings do
      binding match
        case Left(expr)   => env.define(name, Value.VSyntax(expr))
        case Right(exprs) => env.define(name, Value.VSyntaxList(exprs))

  /** Expand a syntax template, returning the expanded Expr and injection bindings for hygiene. */
  def expandSyntaxTemplate(template: Expr, env: Env, pos: Pos): (Expr, Map[String, Value]) =
    // Collect pattern variable names (those bound to VSyntax/VSyntaxList in env)
    val patVarNames = findAllSymbols(template).filter { name =>
      env.lookupOpt(name) match
        case Some(_: Value.VSyntax)     => true
        case Some(_: Value.VSyntaxList) => true
        case _                          => false
    }
    val allSyms    = findAllSymbols(template)
    val introduced = allSyms -- patVarNames -- specialForms - "..."
    val gsMap      = introduced.map(n => n -> gensym(n)).toMap

    // Collect injections: for each gensym, look up the original name in env
    val injections = gsMap.flatMap { (orig, gs) =>
      env.lookupOpt(orig) match
        case Some(v) if !v.isInstanceOf[Value.VSyntax] && !v.isInstanceOf[Value.VSyntaxList] =>
          Some(gs -> v)
        case _ => None
    }

    val expanded = expandTempl(template, env, gsMap)
    (expanded, injections)

  private def findAllSymbols(expr: Expr): Set[String] = expr match
    case Expr.Symbol(name, _)                        => Set(name)
    case Expr.SList(Expr.Symbol("quote", _) :: _, _) => Set.empty
    case Expr.SList(elems, _)                        => elems.flatMap(findAllSymbols).toSet
    case _                                           => Set.empty

  private def expandTempl(
    template: Expr,
    env: Env,
    gsMap: Map[String, String]
  ): Expr = template match
    case Expr.Symbol(name, p) =>
      env.lookupOpt(name) match
        case Some(Value.VSyntax(expr, _)) => expr
        case _ =>
          gsMap.get(name) match
            case Some(gs) => Expr.Symbol(gs, p)
            case None     => template
    case Expr.SList(Expr.Symbol("quote", _) :: _, _) =>
      template // don't touch quoted expressions
    case Expr.SList(elems, p) =>
      Expr.SList(expandTemplList(elems, env, gsMap), p)
    case other => other

  private def expandTemplList(
    elems: List[Expr],
    env: Env,
    gsMap: Map[String, String]
  ): List[Expr] = elems match
    case Nil => Nil
    case elem :: Expr.Symbol("...", _) :: rest =>
      val ellipsisVars = findAllSymbols(elem).filter { name =>
        env.lookupOpt(name) match
          case Some(_: Value.VSyntaxList) => true
          case _                          => false
      }
      if ellipsisVars.isEmpty then expandTemplList(rest, env, gsMap)
      else
        val count = ellipsisVars.map { v =>
          env.lookupOpt(v) match
            case Some(Value.VSyntaxList(lst)) => lst.length
            case _                            => 0
        }.min
        val expanded = (0 until count).toList.map { i =>
          val iterEnv = env.child()
          for v <- ellipsisVars do
            env.lookupOpt(v) match
              case Some(Value.VSyntaxList(lst)) =>
                iterEnv.define(v, Value.VSyntax(lst(i)))
              case _ => ()
          expandTempl(elem, iterEnv, gsMap)
        }
        expanded ++ expandTemplList(rest, env, gsMap)
    case elem :: rest =>
      expandTempl(elem, env, gsMap) :: expandTemplList(rest, env, gsMap)
