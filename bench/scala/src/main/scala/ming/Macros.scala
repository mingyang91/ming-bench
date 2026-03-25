package ming

import scala.collection.mutable

private[ming] object Macros:

  private var gensymCounter = 0L

  def gensym(base: String): String =
    gensymCounter += 1
    s"${base}__hyg_${gensymCounter}"

  val specialForms: Set[String] = Set(
    "if",
    "begin",
    "let",
    "set!",
    "define",
    "lambda",
    "cond",
    "and",
    "or",
    "quote",
    "define-syntax",
    "syntax-rules",
    "let*",
    "letrec",
    "when",
    "unless",
    "syntax-case",
    "syntax",
    "with-syntax",
    "guard",
    "define-record-type",
    "case-lambda",
    "do",
    "letrec*",
    "case"
  )

  // --- Thread-local syntax-case context ---

  private case class SyntaxCtx(
    bindings: Map[String, Either[Expr, List[Expr]]],
    defEnv: Env,
    useEnv: Env
  )

  private val ctxStack = new ThreadLocal[List[SyntaxCtx]]:
    override def initialValue(): List[SyntaxCtx] = Nil

  def pushSyntaxContext(defEnv: Env, useEnv: Env): Unit =
    ctxStack.set(SyntaxCtx(Map.empty, defEnv, useEnv) :: ctxStack.get())

  def popSyntaxContext(): Unit =
    ctxStack.get() match
      case _ :: rest => ctxStack.set(rest)
      case Nil       => ()

  def addSyntaxBindings(bindings: Map[String, Either[Expr, List[Expr]]]): Unit =
    ctxStack.get() match
      case head :: rest =>
        ctxStack.set(head.copy(bindings = head.bindings ++ bindings) :: rest)
      case Nil => ()

  def currentSyntaxBindings: Map[String, Either[Expr, List[Expr]]] =
    ctxStack.get() match
      case head :: _ => head.bindings
      case Nil       => Map.empty

  def currentDefEnv: Option[Env] =
    ctxStack.get() match
      case head :: _ => Some(head.defEnv)
      case Nil       => None

  def currentUseEnv: Option[Env] =
    ctxStack.get() match
      case head :: _ => Some(head.useEnv)
      case Nil       => None

  /** Expand a macro call. Returns (expanded expr, env to evaluate it in). */
  def expandMacro(
    literals: List[String],
    rules: List[(List[Expr], Expr)],
    form: List[Expr],
    defEnv: Env,
    useEnv: Env
  ): (Expr, Env) =
    val inputArgs = form.tail
    rules.iterator
      .flatMap { case (pattern, template) =>
        matchPattern(pattern, inputArgs, literals).map { bindings =>
          val patVars  = collectPatternVars(pattern, literals)
          val freeSyms = collectFreeSymbols(template, patVars)
          val renames  = mutable.Map[String, String]()
          for sym <- freeSyms if !specialForms.contains(sym) do
            val isMacroRef =
              defEnv.lookupOpt(sym).exists(v => v.isInstanceOf[Expr.Macro] || v.isInstanceOf[Expr.TransformerMacro])
            if !isMacroRef then renames(sym) = gensym(sym)

          val expanded = doExpand(template, bindings, renames.toMap)

          for (original, renamed) <- renames do
            defEnv.lookupOpt(original) match
              case Some(_: Expr.Macro) | Some(_: Expr.TransformerMacro) => ()
              case Some(value)                                          => useEnv.define(renamed, value)
              case None                                                 => ()
          (expanded, useEnv)
        }
      }
      .nextOption()
      .getOrElse(throw EvalError("syntax-rules: no matching pattern"))

  /** Expand a syntax template using current syntax-case bindings. */
  def expandSyntaxTemplate(template: Expr, curEnv: Env): (Expr, Env) =
    val bindings = currentSyntaxBindings
    val defEnv   = currentDefEnv.getOrElse(curEnv)
    val useEnv   = currentUseEnv.getOrElse(curEnv)
    val patVars  = bindings.keySet
    val freeSyms = collectFreeSymbols(template, patVars)
    val renames  = mutable.Map[String, String]()
    for sym <- freeSyms if !specialForms.contains(sym) do
      val isMacroRef =
        defEnv.lookupOpt(sym).exists(v => v.isInstanceOf[Expr.Macro] || v.isInstanceOf[Expr.TransformerMacro])
      if !isMacroRef then renames(sym) = gensym(sym)

    val expanded = doExpand(template, bindings, renames.toMap)

    // Inject hygiene renames directly into useEnv (not a child)
    // so that defines in expanded code go into the use-site env
    for (original, renamed) <- renames do
      defEnv.lookupOpt(original) match
        case Some(_: Expr.Macro) | Some(_: Expr.TransformerMacro) => ()
        case Some(value)                                          => useEnv.define(renamed, value)
        case None                                                 => ()

    (expanded, useEnv)

  /** Match a syntax-case pattern against a single input expr. */
  def matchSyntaxCasePattern(
    pattern: Expr,
    input: Expr,
    literals: List[String]
  ): Option[Map[String, Either[Expr, List[Expr]]]] =
    val bindings = mutable.Map[String, Either[Expr, List[Expr]]]()
    if matchSingle(pattern, input, literals, bindings) then Some(bindings.toMap)
    else None

  private def collectPatternVars(pattern: List[Expr], literals: List[String]): Set[String] =
    val vars = Set.newBuilder[String]
    def collect(p: Expr): Unit = p match
      case Expr.Sym(name) if name != "..." && !literals.contains(name) && name != "_" =>
        vars += name
      case Expr.Lst(elems) => elems.foreach(collect)
      case _               => ()
    pattern.foreach(collect)
    vars.result()

  def collectFreeSymbols(template: Expr, patVars: Set[String]): Set[String] = template match
    case Expr.Sym(name) if !patVars.contains(name) && name != "..." => Set(name)
    case Expr.Lst(Expr.Sym("quote") :: _)                           => Set.empty
    case Expr.Lst(elems) => elems.flatMap(e => collectFreeSymbols(e, patVars)).toSet
    case _               => Set.empty

  // --- Pattern matching ---

  private def matchPattern(
    pattern: List[Expr],
    input: List[Expr],
    literals: List[String]
  ): Option[Map[String, Either[Expr, List[Expr]]]] =
    val bindings = mutable.Map[String, Either[Expr, List[Expr]]]()
    if matchElems(pattern, input, literals, bindings) then Some(bindings.toMap)
    else None

  private[ming] def matchElems(
    pattern: List[Expr],
    input: List[Expr],
    literals: List[String],
    bindings: mutable.Map[String, Either[Expr, List[Expr]]]
  ): Boolean =
    val ellipsisIdx = pattern.indexWhere(_ == Expr.Sym("..."))
    if ellipsisIdx < 0 then
      if pattern.length != input.length then return false
      pattern.zip(input).forall((p, i) => matchSingle(p, i, literals, bindings))
    else
      if ellipsisIdx == 0 then return false
      val beforeEllipsis  = pattern.take(ellipsisIdx - 1)
      val ellipsisPattern = pattern(ellipsisIdx - 1)
      val afterEllipsis   = pattern.drop(ellipsisIdx + 1)
      val minRequired     = beforeEllipsis.length + afterEllipsis.length
      if input.length < minRequired then return false
      val repeatCount = input.length - minRequired

      if !beforeEllipsis.zip(input.take(beforeEllipsis.length)).forall((p, i) => matchSingle(p, i, literals, bindings))
      then return false

      val repeatedInputs   = input.slice(beforeEllipsis.length, beforeEllipsis.length + repeatCount)
      val ellipsisVars     = collectSinglePatternVars(ellipsisPattern, literals)
      val ellipsisBindings = mutable.Map[String, List[Expr]]()
      for v <- ellipsisVars do ellipsisBindings(v) = Nil

      val allRepeatsMatch = repeatedInputs.forall { elem =>
        val tempBindings = mutable.Map[String, Either[Expr, List[Expr]]]()
        val matched      = matchSingle(ellipsisPattern, elem, literals, tempBindings)
        if matched then
          for (k, v) <- tempBindings do
            v match
              case Left(expr) =>
                ellipsisBindings(k) = ellipsisBindings.getOrElse(k, Nil) :+ expr
              case _ => ()
        matched
      }

      if !allRepeatsMatch then false
      else
        for (k, vs) <- ellipsisBindings do bindings(k) = Right(vs)
        val afterInput = input.drop(beforeEllipsis.length + repeatCount)
        afterEllipsis.zip(afterInput).forall((p, i) => matchSingle(p, i, literals, bindings))

  private def collectSinglePatternVars(pattern: Expr, literals: List[String]): Set[String] = pattern match
    case Expr.Sym(name) if name != "..." && !literals.contains(name) && name != "_" => Set(name)
    case Expr.Lst(elems) => elems.flatMap(e => collectSinglePatternVars(e, literals)).toSet
    case _               => Set.empty

  private[ming] def matchSingle(
    pattern: Expr,
    input: Expr,
    literals: List[String],
    bindings: mutable.Map[String, Either[Expr, List[Expr]]]
  ): Boolean = pattern match
    case Expr.Sym("_") => true
    case Expr.Sym(name) if literals.contains(name) =>
      input match
        case Expr.Sym(n) => n == name
        case _           => false
    case Expr.Sym(name) if name != "..." =>
      bindings(name) = Left(input)
      true
    case Expr.Lst(pelems) =>
      input match
        case Expr.Lst(ielems) => matchElems(pelems, ielems, literals, bindings)
        case _                => false
    case _ => false

  // --- Template expansion ---

  def doExpand(
    template: Expr,
    bindings: Map[String, Either[Expr, List[Expr]]],
    renames: Map[String, String]
  ): Expr = template match
    case Expr.Sym(name) if bindings.contains(name) =>
      bindings(name) match
        case Left(v)  => v
        case Right(_) => throw EvalError(s"syntax-rules: ellipsis variable $name used without ellipsis")
    case Expr.Sym(name) if renames.contains(name) =>
      Expr.Sym(renames(name))
    case Expr.Sym(_) => template
    case Expr.Lst(elems) =>
      Expr.Lst(expandList(elems, bindings, renames))
    case _ => template

  private def expandList(
    elems: List[Expr],
    bindings: Map[String, Either[Expr, List[Expr]]],
    renames: Map[String, String]
  ): List[Expr] =
    val result = List.newBuilder[Expr]
    var i      = 0
    while i < elems.length do
      if i + 1 < elems.length && elems(i + 1) == Expr.Sym("...") then
        val elem     = elems(i)
        val usedVars = findEllipsisVars(elem, bindings)
        if usedVars.isEmpty then throw EvalError("syntax-rules: no ellipsis variable in repeated template")
        val count = bindings(usedVars.head) match
          case Right(vs) => vs.length
          case _         => throw EvalError("syntax-rules: not an ellipsis binding")
        for j <- 0 until count do
          val iterBindings = bindings.map { case (k, v) =>
            if usedVars.contains(k) then
              v match
                case Right(vs) => (k, Left(vs(j)))
                case other     => (k, other)
            else (k, v)
          }
          result += doExpand(elem, iterBindings, renames)
        i += 2
      else
        result += doExpand(elems(i), bindings, renames)
        i += 1
    result.result()

  private def findEllipsisVars(template: Expr, bindings: Map[String, Either[Expr, List[Expr]]]): Set[String] =
    template match
      case Expr.Sym(name) if bindings.get(name).exists(_.isRight) => Set(name)
      case Expr.Lst(elems) => elems.flatMap(e => findEllipsisVars(e, bindings)).toSet
      case _               => Set.empty
