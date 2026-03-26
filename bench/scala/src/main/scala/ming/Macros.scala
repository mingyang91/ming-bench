package ming

import scala.collection.mutable

object Macros:
  private[ming] var gensymCounter = 0L

  private[ming] def gensym(base: String): String =
    gensymCounter += 1
    s"${base}__m${gensymCounter}"

  // Binding from pattern matching: single expr or list (for ellipsis)
  sealed trait Binding
  case class SingleBinding(expr: Expr)      extends Binding
  case class ListBinding(exprs: List[Expr]) extends Binding

  // Collect pattern variable names
  def patternVars(pattern: Expr, literals: Set[String], macroName: String): Set[String] =
    pattern match
      case Symbol(name, _) if name != "_" && name != "..." && !literals.contains(name) && name != macroName =>
        Set(name)
      case SList(elems, _) =>
        elems
          .filterNot { case Symbol("...", _) => true; case _ => false }
          .flatMap(e => patternVars(e, literals, macroName))
          .toSet
      case _ => Set.empty

  // Match input against pattern, return bindings or None
  def matchPattern(
    input: Expr,
    pattern: Expr,
    literals: Set[String],
    macroName: String
  ): Option[Map[String, Binding]] =
    (pattern, input) match
      case (Symbol("_", _), _) => Some(Map.empty)
      case (Symbol(name, _), _) if name == macroName =>
        input match
          case Symbol(n, _) if n == macroName => Some(Map.empty)
          case _                              => None
      case (Symbol(name, _), _) if literals.contains(name) =>
        input match
          case Symbol(n, _) if n == name => Some(Map.empty)
          case _                         => None
      case (Symbol(name, _), _) if name != "..." =>
        Some(Map(name -> SingleBinding(input)))
      case (SList(patElems, _), SList(inpElems, _)) =>
        matchListPattern(inpElems, patElems, literals, macroName)
      case _ => None

  private def matchListPattern(
    inputs: List[Expr],
    patterns: List[Expr],
    literals: Set[String],
    macroName: String
  ): Option[Map[String, Binding]] =
    val ellipsisIdx = patterns.indexWhere { case Symbol("...", _) => true; case _ => false }

    if ellipsisIdx < 0 then
      if inputs.size != patterns.size then return None
      val bindings = mutable.Map[String, Binding]()
      for (p, i) <- patterns.zip(inputs) do
        matchPattern(i, p, literals, macroName) match
          case Some(b) => bindings ++= b
          case None    => return None
      Some(bindings.toMap)
    else
      val beforeEllipsis = patterns.take(ellipsisIdx - 1)
      val repeatingPat   = patterns(ellipsisIdx - 1)
      val afterEllipsis  = patterns.drop(ellipsisIdx + 1)
      val minRequired    = beforeEllipsis.size + afterEllipsis.size
      if inputs.size < minRequired then return None

      val bindings = mutable.Map[String, Binding]()

      // Match fixed patterns before ellipsis
      for (p, i) <- beforeEllipsis.zip(inputs) do
        matchPattern(i, p, literals, macroName) match
          case Some(b) => bindings ++= b
          case None    => return None

      // Match fixed patterns after ellipsis
      val afterInputs = inputs.drop(inputs.size - afterEllipsis.size)
      for (p, i) <- afterEllipsis.zip(afterInputs) do
        matchPattern(i, p, literals, macroName) match
          case Some(b) => bindings ++= b
          case None    => return None

      // Match repeating pattern against middle inputs
      val middleInputs = inputs.drop(beforeEllipsis.size).dropRight(afterEllipsis.size)
      val repVars      = patternVars(repeatingPat, literals, macroName)
      val repBindings  = mutable.Map[String, mutable.ListBuffer[Expr]]()
      for v <- repVars do repBindings(v) = mutable.ListBuffer.empty

      for inp <- middleInputs do
        matchPattern(inp, repeatingPat, literals, macroName) match
          case Some(b) =>
            for (k, v) <- b do
              v match
                case SingleBinding(e) => repBindings.getOrElseUpdate(k, mutable.ListBuffer.empty) += e
                case _                => ()
          case None => return None

      for (k, vs) <- repBindings do bindings(k) = ListBinding(vs.toList)
      Some(bindings.toMap)

  private def expandEllipsis(
    tmpl: Expr,
    bindings: Map[String, Binding],
    renaming: Map[String, String]
  ): List[Expr] =
    val vars = templateVars(tmpl).filter(v =>
      bindings.get(v) match
        case Some(_: ListBinding) => true;
        case _                    => false
    )
    if vars.isEmpty then return Nil
    val listLen = bindings(vars.head) match
      case ListBinding(es) => es.size;
      case _               => 0
    (0 until listLen).map { j =>
      val iterBindings = bindings.map((k, v) => resolveEllipsisBinding(k, v, vars, j))
      substitute(tmpl, iterBindings, renaming)
    }.toList

  private def resolveEllipsisBinding(
    k: String,
    v: Binding,
    vars: Set[String],
    idx: Int
  ): (String, Binding) =
    v match
      case ListBinding(es) if vars.contains(k) => (k, SingleBinding(es(idx)))
      case other                               => (k, other)

  private def isEllipsis(e: Expr): Boolean = e match
    case Symbol("...", _) => true
    case _                => false

  // Substitute pattern variables in template, applying renaming for hygiene
  def substitute(template: Expr, bindings: Map[String, Binding], renaming: Map[String, String]): Expr =
    template match
      case Symbol(name, pos) =>
        renaming.get(name) match
          case Some(newName) => Symbol(newName, pos)
          case None =>
            bindings.get(name) match
              case Some(SingleBinding(e)) => e
              case _                      => template
      case SList(elems, pos) =>
        val result = mutable.ListBuffer[Expr]()
        var i      = 0
        while i < elems.size do
          if i + 1 < elems.size && isEllipsis(elems(i + 1)) then
            result ++= expandEllipsis(elems(i), bindings, renaming)
            i += 2
          else
            result += substitute(elems(i), bindings, renaming)
            i += 1
        SList(result.toList, pos)
      case _ => template

  private def templateVars(template: Expr): Set[String] =
    template match
      case Symbol(name, _) => Set(name)
      case SList(elems, _) => elems.flatMap(templateVars).toSet
      case _               => Set.empty

  // Find identifiers introduced by let/lambda bindings in the template
  private[ming] def findIntroducedBindings(template: Expr, patVars: Set[String]): Set[String] =
    template match
      case SList(Symbol("let", _) :: SList(bindings, _) :: body, _) =>
        val letVars = bindings.flatMap {
          case SList(Symbol(name, _) :: _, _) if !patVars.contains(name) => Some(name)
          case _                                                         => None
        }.toSet
        letVars ++ (bindings ++ body).flatMap(c => findIntroducedBindings(c, patVars))
      case SList(Symbol("lambda", _) :: SList(params, _) :: body, _) =>
        val lambdaVars = params.flatMap {
          case Symbol(name, _) if !patVars.contains(name) => Some(name)
          case _                                          => None
        }.toSet
        lambdaVars ++ body.flatMap(c => findIntroducedBindings(c, patVars))
      case SList(elems, _) =>
        elems.flatMap(e => findIntroducedBindings(e, patVars)).toSet
      case _ => Set.empty

  // Find free identifier references in template (not pattern vars, not special forms, not bound)
  private val specialForms = Set(
    "if",
    "begin",
    "let",
    "lambda",
    "define",
    "set!",
    "cond",
    "and",
    "or",
    "quote",
    "define-syntax",
    "syntax-rules"
  )

  private[ming] def findFreeRefs(
    template: Expr,
    patVars: Set[String],
    literals: Set[String],
    bound: Set[String]
  ): Set[String] =
    template match
      case Symbol(name, _)
          if !patVars.contains(name) && !literals.contains(name) &&
            !specialForms.contains(name) && !bound.contains(name) && name != "..." =>
        Set(name)
      case SList(Symbol("let", _) :: SList(bindings, _) :: body, _) =>
        val bindingRefs = bindings.flatMap {
          case SList(_ :: init :: Nil, _) => findFreeRefs(init, patVars, literals, bound)
          case _                          => Set.empty[String]
        }.toSet
        val letVars = bindings.flatMap {
          case SList(Symbol(name, _) :: _, _) => Some(name)
          case _                              => None
        }.toSet
        bindingRefs ++ body.flatMap(e => findFreeRefs(e, patVars, literals, bound ++ letVars)).toSet
      case SList(Symbol("lambda", _) :: SList(params, _) :: body, _) =>
        val paramNames = params.flatMap { case Symbol(n, _) => Some(n); case _ => None }.toSet
        body.flatMap(e => findFreeRefs(e, patVars, literals, bound ++ paramNames)).toSet
      case SList(elems, _) =>
        elems.flatMap(e => findFreeRefs(e, patVars, literals, bound)).toSet
      case _ => Set.empty

  // Expand a macro call
  def expand(macro_ : SchemeMacro, input: SList, useEnv: Env): Expr =
    val SchemeMacro(literals, rules, defEnv) = macro_
    val litSet                               = literals.toSet
    val macroName = input.elems.head match
      case Symbol(n, _) => n;
      case _            => ""

    for (pattern, template) <- rules do
      matchPattern(input, pattern, litSet, macroName) match
        case Some(bindings) =>
          val patVars = bindings.keySet

          // Hygiene: rename identifiers introduced by binding forms
          val introduced = findIntroducedBindings(template, patVars)
          val renaming   = introduced.map(name => name -> gensym(name)).toMap

          // Definition-site binding: rename free refs and bind gensyms to defEnv values
          val freeRefs        = findFreeRefs(template, patVars, litSet, Set.empty)
          val defSiteRenaming = mutable.Map[String, String]()
          for ref <- freeRefs do
            try
              val v     = defEnv.get(ref)
              val fresh = gensym(ref)
              defSiteRenaming(ref) = fresh
              useEnv.set(fresh, v)
            catch case _: EvalError => () // not in defEnv, leave as-is

          val allRenaming = renaming ++ defSiteRenaming
          return substitute(template, bindings, allRenaming)
        case None => ()

    throw new EvalError(s"no matching pattern for macro $macroName")

  /** Expand a syntax-quote template using syntax bindings from the environment. */
  def expandSyntaxQuote(template: Expr, env: Env): SchemeSyntax =
    val templateSyms = collectAllSymbols(template)
    val patVarNames  = mutable.Set[String]()
    val bindings     = mutable.Map[String, Binding]()

    for name <- templateSyms do
      try
        env.get(name) match
          case SchemeSyntax(expr) =>
            patVarNames += name
            bindings(name) = SingleBinding(expr)
          case SchemeSyntaxList(exprs) =>
            patVarNames += name
            bindings(name) = ListBinding(exprs)
          case _ => ()
      catch case _: EvalError => ()

    // Hygiene: rename identifiers introduced by binding forms in the template
    val introduced = findIntroducedBindings(template, patVarNames.toSet)
    val renaming   = introduced.map(name => name -> gensym(name)).toMap

    val expanded = substitute(template, bindings.toMap, renaming)
    SchemeSyntax(expanded)

  private def collectAllSymbols(expr: Expr): Set[String] =
    expr match
      case Symbol(name, _) => Set(name)
      case SList(elems, _) => elems.flatMap(collectAllSymbols).toSet
      case _               => Set.empty
