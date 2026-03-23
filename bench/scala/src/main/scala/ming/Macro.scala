package ming

import SchemeValue.*

/** Hygienic macro expansion for syntax-rules. */
object Macro:

  type Bindings = Map[String, Either[SchemeValue, List[SchemeValue]]]

  def expand(
    macroName: String,
    literals: Set[String],
    rules: List[(SchemeValue, SchemeValue)],
    defEnv: Environment,
    form: SchemeValue
  ): SchemeValue =
    rules.iterator
      .flatMap { case (pattern, template) =>
        matchForm(pattern, form, literals, macroName).map { bindings =>
          val patVars = collectAllPatternVars(pattern, literals, macroName)
          expandTemplate(template, bindings, defEnv, patVars)
        }
      }
      .nextOption()
      .getOrElse(throw new EvalError(s"$macroName: no matching pattern"))

  private def matchForm(
    pattern: SchemeValue,
    input: SchemeValue,
    literals: Set[String],
    macroName: String
  ): Option[Bindings] =
    (pattern, input) match
      case (ListVal(pElems, _), ListVal(iElems, _)) =>
        matchListElems(pElems, iElems, literals, macroName)
      case _ => None

  private def matchElem(
    pattern: SchemeValue,
    input: SchemeValue,
    literals: Set[String],
    macroName: String
  ): Option[Bindings] =
    (pattern, input) match
      case (SymbolVal(name, _), _) if name == macroName =>
        Some(Map.empty)
      case (SymbolVal(name, _), SymbolVal(iName, _)) if literals.contains(name) =>
        if name == iName then Some(Map.empty) else None
      case (SymbolVal(name, _), _) if literals.contains(name) =>
        None
      case (SymbolVal(name, _), _) if name != "..." =>
        Some(Map(name -> Left(input)))
      case (ListVal(pElems, _), ListVal(iElems, _)) =>
        matchListElems(pElems, iElems, literals, macroName)
      case (BoolVal(a, _), BoolVal(b, _)) if a == b => Some(Map.empty)
      case (IntVal(a, _), IntVal(b, _)) if a == b   => Some(Map.empty)
      case _                                        => None

  private def matchListElems(
    pElems: List[SchemeValue],
    iElems: List[SchemeValue],
    literals: Set[String],
    macroName: String
  ): Option[Bindings] =
    val ellipsisIdx = pElems.indexWhere {
      case SymbolVal("...", _) => true
      case _                   => false
    }

    if ellipsisIdx < 0 then matchFixed(pElems, iElems, literals, macroName)
    else matchWithEllipsis(pElems, iElems, ellipsisIdx, literals, macroName)

  private def matchFixed(
    pElems: List[SchemeValue],
    iElems: List[SchemeValue],
    literals: Set[String],
    macroName: String
  ): Option[Bindings] =
    if pElems.length != iElems.length then return None
    var bindings: Bindings = Map.empty
    var i                  = 0
    while i < pElems.length do
      matchElem(pElems(i), iElems(i), literals, macroName) match
        case Some(b) => bindings = bindings ++ b
        case None    => return None
      i += 1
    Some(bindings)

  private def matchWithEllipsis(
    pElems: List[SchemeValue],
    iElems: List[SchemeValue],
    ellipsisIdx: Int,
    literals: Set[String],
    macroName: String
  ): Option[Bindings] =
    val beforeCount = ellipsisIdx - 1
    val afterElems  = pElems.drop(ellipsisIdx + 1)
    val minRequired = beforeCount + afterElems.length
    if iElems.length < minRequired then return None

    var bindings: Bindings = Map.empty

    // Fixed elements before repeated pattern
    var i = 0
    while i < beforeCount do
      matchElem(pElems(i), iElems(i), literals, macroName) match
        case Some(b) => bindings = bindings ++ b
        case None    => return None
      i += 1

    // Fixed elements after ellipsis
    val afterStart = iElems.length - afterElems.length
    i = 0
    while i < afterElems.length do
      matchElem(afterElems(i), iElems(afterStart + i), literals, macroName) match
        case Some(b) => bindings = bindings ++ b
        case None    => return None
      i += 1

    // Repeated elements
    val repeatedPattern = pElems(ellipsisIdx - 1)
    val repeatedInputs  = iElems.slice(beforeCount, afterStart)
    val patVars         = collectPatternVars(repeatedPattern, literals, macroName)

    var ellipsisBindings: Map[String, List[SchemeValue]] =
      patVars.map(_ -> List.empty[SchemeValue]).toMap

    var ri = 0
    while ri < repeatedInputs.length do
      matchElem(repeatedPattern, repeatedInputs(ri), literals, macroName) match
        case Some(b) =>
          for (k, v) <- b do
            val existing = ellipsisBindings.getOrElse(k, Nil)
            v match
              case Left(sv)   => ellipsisBindings = ellipsisBindings.updated(k, existing :+ sv)
              case Right(lst) => ellipsisBindings = ellipsisBindings.updated(k, existing ++ lst)
        case None => return None
      ri += 1

    for (k, vs) <- ellipsisBindings do bindings = bindings + (k -> Right(vs))

    Some(bindings)

  private def collectPatternVars(
    pattern: SchemeValue,
    literals: Set[String],
    macroName: String
  ): Set[String] =
    pattern match
      case SymbolVal(name, _) if name != macroName && !literals.contains(name) && name != "..." =>
        Set(name)
      case ListVal(elems, _) =>
        elems.flatMap(collectPatternVars(_, literals, macroName)).toSet
      case _ => Set.empty

  private def collectAllPatternVars(
    pattern: SchemeValue,
    literals: Set[String],
    macroName: String
  ): Set[String] =
    pattern match
      case ListVal(elems, _) =>
        elems.flatMap(collectPatternVars(_, literals, macroName)).toSet
      case _ => collectPatternVars(pattern, literals, macroName)

  /** Expand a template, substituting pattern variables and applying hygiene. */
  private def expandTemplate(
    template: SchemeValue,
    bindings: Bindings,
    defEnv: Environment,
    patternVars: Set[String]
  ): SchemeValue =
    template match
      case SymbolVal(name, _) if bindings.contains(name) =>
        bindings(name) match
          case Left(value) => value
          case Right(_)    => throw new EvalError(s"pattern variable $name used without ellipsis")

      // Hygiene: free template variables resolve to definition-site values
      case SymbolVal(name, _) if !patternVars.contains(name) =>
        defEnv.get(name) match
          case Some(_: SyntaxRulesVal) => template
          case Some(value)             => value
          case None                    => template

      case ListVal(elems, pos) =>
        ListVal(expandListElems(elems, bindings, defEnv, patternVars), pos)

      case _ => template

  private def expandListElems(
    elems: List[SchemeValue],
    bindings: Bindings,
    defEnv: Environment,
    patternVars: Set[String]
  ): List[SchemeValue] =
    val result = List.newBuilder[SchemeValue]
    var i      = 0
    while i < elems.length do
      if i + 1 < elems.length && isEllipsis(elems(i + 1)) then
        val subTemplate  = elems(i)
        val tplVars      = collectTemplateVars(subTemplate)
        val ellipsisVars = tplVars.filter(v => bindings.get(v).exists(_.isRight))

        val count =
          if ellipsisVars.isEmpty then 0
          else
            bindings(ellipsisVars.head) match
              case Right(lst) => lst.length
              case _          => 0

        var j = 0
        while j < count do
          val iterBindings = bindings.map { (k, v) =>
            if ellipsisVars.contains(k) then
              v match
                case Right(lst) => (k, Left(lst(j)))
                case other      => (k, other)
            else (k, v)
          }
          result += expandTemplate(subTemplate, iterBindings, defEnv, patternVars)
          j += 1
        i += 2
      else
        result += expandTemplate(elems(i), bindings, defEnv, patternVars)
        i += 1
    result.result()

  private def isEllipsis(sv: SchemeValue): Boolean = sv match
    case SymbolVal("...", _) => true
    case _                   => false

  private def collectTemplateVars(template: SchemeValue): Set[String] =
    template match
      case SymbolVal(name, _) if name != "..." => Set(name)
      case ListVal(elems, _)                   => elems.flatMap(collectTemplateVars).toSet
      case _                                   => Set.empty
