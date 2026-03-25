package ming

import scala.collection.mutable

object Macro:
  private var counter = 0L

  private def gensym(base: String): String =
    counter += 1
    s"${base}__m${counter}"

  private val specialForms = Set(
    "if",
    "define",
    "lambda",
    "and",
    "or",
    "let",
    "set!",
    "begin",
    "cond",
    "quote",
    "define-syntax",
    "syntax-rules",
    "syntax-case",
    "syntax",
    "with-syntax",
    "let*",
    "letrec",
    "letrec*",
    "case-lambda",
    "define-record-type",
    "case",
    "do",
    "guard"
  )

  /** Bindings from pattern matching: name -> Left(single value) or Right(ellipsis list) */
  type Bindings = Map[String, Either[SchemeVal, List[SchemeVal]]]

  /** Match a pattern against an input value. Public API for syntax-case. */
  def matchFull(pattern: SchemeVal, input: SchemeVal, literals: Set[String]): Option[Bindings] =
    matchOne(pattern, input, literals)

  /** Instantiate a template with bindings and hygiene. Public API for syntax-case. */
  def expandTemplate(
    template: SchemeVal,
    bindings: Bindings,
    defEnv: Env,
    defBound: Set[String] = Set.empty
  ): SchemeVal =
    if defBound.isEmpty then instantiateWithHygiene(template, bindings, defEnv)
    else
      val patVars      = bindings.keySet
      val freeSyms     = collectFreeSymbols(template, patVars)
      val bindingNames = collectBindingNames(template, patVars)
      val defValues    = mutable.HashMap[String, SchemeVal]()
      val gensymMap    = mutable.HashMap[String, String]()
      for sym <- freeSyms do
        if !specialForms.contains(sym) && !Builtins.names.contains(sym) then
          if defBound.contains(sym) then
            defEnv.lookup(sym) match
              case Some(_: SchemeVal.SMacro) => ()
              case Some(v)                   => defValues(sym) = v
              case None                      => ()
          else if bindingNames.contains(sym) then gensymMap(sym) = gensym(sym)
          // else: leave as-is for use-site resolution
      instantiate(template, bindings, defValues.toMap, gensymMap.toMap)

  /** Collect symbols that appear in binding positions (let/lambda) in a template. */
  private def collectBindingNames(template: SchemeVal, patVars: Set[String]): Set[String] =
    template match
      case SchemeVal.SList(SchemeVal.SSymbol(form) :: SchemeVal.SList(bindings) :: body)
          if Set("let", "let*", "letrec", "letrec*").contains(form) =>
        val names = bindings.flatMap {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: _) if !patVars.contains(n) => Some(n)
          case _                                                                  => None
        }.toSet
        names ++ body.flatMap(collectBindingNames(_, patVars)).toSet ++
          bindings.flatMap {
            case SchemeVal.SList(_ :: v :: Nil) => collectBindingNames(v, patVars)
            case _                              => Set.empty
          }
      case SchemeVal.SList(SchemeVal.SSymbol("lambda") :: SchemeVal.SList(params) :: body) =>
        val names = params.collect {
          case SchemeVal.SSymbol(n) if !patVars.contains(n) => n
        }.toSet
        names ++ body.flatMap(collectBindingNames(_, patVars)).toSet
      case SchemeVal.SList(SchemeVal.SSymbol("quote") :: _) => Set.empty
      case SchemeVal.SList(elems) =>
        elems.flatMap(collectBindingNames(_, patVars)).toSet
      case _ => Set.empty

  /** Expand a macro application. Tries each clause until one matches. */
  def expand(macro_ : SchemeVal.SMacro, form: SchemeVal): SchemeVal =
    val formElems = form match
      case SchemeVal.SList(es) => es
      case _                   => throw new EvalError("macro application must be a list")
    for (pattern, template) <- macro_.clauses do
      pattern match
        case SchemeVal.SList(patElems) =>
          tryMatch(patElems.tail, formElems.tail, macro_.literals) match
            case Some(bindings) =>
              return instantiateWithHygiene(template, bindings, macro_.defEnv)
            case None => ()
        case _ => ()
    throw new EvalError("no matching clause in syntax-rules")

  /** Try to match input elements against pattern elements. */
  private def tryMatch(
    patElems: List[SchemeVal],
    inElems: List[SchemeVal],
    literals: Set[String]
  ): Option[Bindings] =
    val ellipsisIdx = patElems.indexWhere {
      case SchemeVal.SSymbol("...") => true
      case _                        => false
    }

    if ellipsisIdx < 0 then
      if patElems.length != inElems.length then return None
      val bindings = mutable.HashMap[String, Either[SchemeVal, List[SchemeVal]]]()
      for (p, i) <- patElems.zip(inElems) do
        matchOne(p, i, literals) match
          case Some(b) => bindings ++= b
          case None    => return None
      Some(bindings.toMap)
    else
      if ellipsisIdx == 0 then return None
      val fixedBefore = patElems.take(ellipsisIdx - 1)
      val ellipsisPat = patElems(ellipsisIdx - 1)
      val fixedAfter  = patElems.drop(ellipsisIdx + 1)
      val minRequired = fixedBefore.length + fixedAfter.length
      if inElems.length < minRequired then return None

      val bindings = mutable.HashMap[String, Either[SchemeVal, List[SchemeVal]]]()

      // Match fixed elements before ellipsis
      for (p, i) <- fixedBefore.zip(inElems) do
        matchOne(p, i, literals) match
          case Some(b) => bindings ++= b
          case None    => return None

      // Match ellipsis elements
      val ellipsisInputs = inElems.slice(fixedBefore.length, inElems.length - fixedAfter.length)
      val ellipsisVars   = collectPatternVars(ellipsisPat, literals)
      val accum          = mutable.HashMap[String, mutable.ListBuffer[SchemeVal]]()
      for v <- ellipsisVars do accum(v) = mutable.ListBuffer()

      for inp <- ellipsisInputs do
        matchOne(ellipsisPat, inp, literals) match
          case Some(b) =>
            for (name, value) <- b do
              value match
                case Left(v) => accum.getOrElseUpdate(name, mutable.ListBuffer()) += v
                case _       => ()
          case None => return None

      for (name, values) <- accum do bindings(name) = Right(values.toList)

      // Match fixed elements after ellipsis
      for (p, i) <- fixedAfter.zip(inElems.takeRight(fixedAfter.length)) do
        matchOne(p, i, literals) match
          case Some(b) => bindings ++= b
          case None    => return None

      Some(bindings.toMap)

  /** Match a single pattern element against a single input element. */
  private def matchOne(
    pattern: SchemeVal,
    input: SchemeVal,
    literals: Set[String]
  ): Option[Bindings] =
    pattern match
      case SchemeVal.SSymbol("_") => Some(Map.empty)
      case SchemeVal.SSymbol(name) if literals.contains(name) =>
        input match
          case SchemeVal.SSymbol(n) if n == name => Some(Map.empty)
          case _                                 => None
      case SchemeVal.SSymbol(name) =>
        Some(Map(name -> Left(input)))
      case SchemeVal.SList(pElems) =>
        input match
          case SchemeVal.SList(iElems) => tryMatch(pElems, iElems, literals)
          case _                       => None
      case SchemeVal.SBool(a) =>
        input match
          case SchemeVal.SBool(b) if a == b => Some(Map.empty)
          case _                            => None
      case SchemeVal.SInt(a) =>
        input match
          case SchemeVal.SInt(b) if a == b => Some(Map.empty)
          case _                           => None
      case _ => None

  /** Collect all pattern variable names from a pattern element. */
  private def collectPatternVars(pattern: SchemeVal, literals: Set[String]): Set[String] =
    pattern match
      case SchemeVal.SSymbol(name) if name != "_" && name != "..." && !literals.contains(name) =>
        Set(name)
      case SchemeVal.SList(elems) => elems.flatMap(collectPatternVars(_, literals)).toSet
      case _                      => Set.empty

  /** Instantiate template with bindings and hygiene (definition-site binding preservation). */
  private def instantiateWithHygiene(
    template: SchemeVal,
    bindings: Bindings,
    defEnv: Env
  ): SchemeVal =
    val patVars  = bindings.keySet
    val freeSyms = collectFreeSymbols(template, patVars)

    // For free symbols in def env that are not builtins/special forms, capture their values
    val defValues = mutable.HashMap[String, SchemeVal]()
    // For free symbols NOT in def env, rename with gensym for hygiene
    val gensymMap = mutable.HashMap[String, String]()

    for sym <- freeSyms do
      if !specialForms.contains(sym) && !Builtins.names.contains(sym) then
        defEnv.lookup(sym) match
          case Some(_: SchemeVal.SMacro) => () // don't inline macros; let normal eval find them
          case Some(v)                   => defValues(sym) = v
          case None                      => gensymMap(sym) = gensym(sym)

    instantiate(template, bindings, defValues.toMap, gensymMap.toMap)

  /** Collect non-pattern-variable, non-ellipsis symbols from a template. */
  private def collectFreeSymbols(template: SchemeVal, patVars: Set[String]): Set[String] =
    template match
      case SchemeVal.SSymbol(name) if !patVars.contains(name) && name != "..." =>
        Set(name)
      case SchemeVal.SList(SchemeVal.SSymbol("quote") :: _) =>
        Set("quote") // quote is free but its contents are literal data
      case SchemeVal.SList(elems) =>
        elems.flatMap(collectFreeSymbols(_, patVars)).toSet
      case _ => Set.empty

  /** Substitute pattern variables, apply hygiene, expand ellipsis. */
  private def instantiate(
    template: SchemeVal,
    bindings: Bindings,
    defValues: Map[String, SchemeVal],
    gensymMap: Map[String, String]
  ): SchemeVal =
    template match
      case SchemeVal.SSymbol(name) if bindings.contains(name) =>
        bindings(name) match
          case Left(v)   => v
          case Right(vs) => SchemeVal.SList(vs)
      case SchemeVal.SSymbol(name) if defValues.contains(name) =>
        defValues(name)
      case SchemeVal.SSymbol(name) if gensymMap.contains(name) =>
        SchemeVal.SSymbol(gensymMap(name))
      case SchemeVal.SList(elems) =>
        SchemeVal.SList(instantiateList(elems, bindings, defValues, gensymMap))
      case other => other

  /** Instantiate a list of template elements, handling ellipsis expansion. */
  private def instantiateList(
    elems: List[SchemeVal],
    bindings: Bindings,
    defValues: Map[String, SchemeVal],
    gensymMap: Map[String, String]
  ): List[SchemeVal] =
    elems match
      case Nil => Nil
      case elem :: SchemeVal.SSymbol("...") :: rest =>
        val ellipsisVars = findEllipsisVars(elem, bindings)
        if ellipsisVars.isEmpty then instantiateList(rest, bindings, defValues, gensymMap)
        else
          val listLen = bindings(ellipsisVars.head) match
            case Right(vs) => vs.length
            case Left(_)   => 1
          val expanded = (0 until listLen).toList.map { i =>
            val iterBindings = bindings.map {
              case (name, Right(vs)) if ellipsisVars.contains(name) =>
                (name, Left(vs(i)): Either[SchemeVal, List[SchemeVal]])
              case other => other
            }
            instantiate(elem, iterBindings, defValues, gensymMap)
          }
          expanded ++ instantiateList(rest, bindings, defValues, gensymMap)
      case elem :: rest =>
        instantiate(elem, bindings, defValues, gensymMap) ::
          instantiateList(rest, bindings, defValues, gensymMap)

  /** Find pattern variables in a template element that have ellipsis (list) bindings. */
  private def findEllipsisVars(template: SchemeVal, bindings: Bindings): Set[String] =
    template match
      case SchemeVal.SSymbol(name) =>
        bindings.get(name) match
          case Some(Right(_)) => Set(name)
          case _              => Set.empty
      case SchemeVal.SList(elems) =>
        elems.flatMap(findEllipsisVars(_, bindings)).toSet
      case _ => Set.empty
