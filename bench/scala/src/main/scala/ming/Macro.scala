package ming

import scala.collection.mutable
import java.util.concurrent.atomic.AtomicLong

object Macro:
  private val counter = new AtomicLong(0L)

  private def gensym(base: String): String =
    val n = counter.incrementAndGet()
    s"${base}__m${n}"

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
    MacroMatch.matchOne(pattern, input, literals)

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
              case Some(_: SchemeVal.SMacro)                               => ()
              case Some(SchemeVal.SSymbol(n)) if n.startsWith("__record-") => ()
              case Some(v)                                                 => defValues(sym) = v
              case None                                                    => ()
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
      case SchemeVal.SPair(cell) =>
        collectBindingNames(cell.car, patVars) ++ collectBindingNames(cell.cdr, patVars)
      case _ => Set.empty

  /** Expand a macro application. Tries each clause until one matches. */
  def expand(macro_ : SchemeVal.SMacro, form: SchemeVal): SchemeVal =
    val (formElems, _) = MacroMatch.flattenVal(form)
    for (pattern, template) <- macro_.clauses do
      val (patElems, patTail) = MacroMatch.flattenVal(pattern)
      MacroMatch.tryMatchDotted(patElems.tail, patTail, formElems.tail, None, macro_.literals) match
        case Some(bindings) =>
          return instantiateWithHygiene(template, bindings, macro_.defEnv)
        case None => ()
    throw new EvalError("no matching clause in syntax-rules")

  /** Instantiate template with bindings and hygiene (definition-site binding preservation). */
  private def instantiateWithHygiene(
    template: SchemeVal,
    bindings: Bindings,
    defEnv: Env
  ): SchemeVal =
    val patVars  = bindings.keySet
    val freeSyms = collectFreeSymbols(template, patVars)

    val defValues = mutable.HashMap[String, SchemeVal]()
    val gensymMap = mutable.HashMap[String, String]()

    for sym <- freeSyms do
      if !specialForms.contains(sym) && !Builtins.names.contains(sym) then
        defEnv.lookup(sym) match
          case Some(_: SchemeVal.SMacro)                               => ()
          case Some(SchemeVal.SSymbol(n)) if n.startsWith("__record-") => ()
          case Some(v)                                                 => defValues(sym) = v
          case None                                                    => gensymMap(sym) = gensym(sym)

    instantiate(template, bindings, defValues.toMap, gensymMap.toMap)

  /** Collect non-pattern-variable, non-ellipsis symbols from a template. */
  private def collectFreeSymbols(template: SchemeVal, patVars: Set[String]): Set[String] =
    template match
      case SchemeVal.SSymbol(name) if !patVars.contains(name) && name != "..." =>
        Set(name)
      case SchemeVal.SList(SchemeVal.SSymbol("quote") :: _) =>
        Set("quote")
      case SchemeVal.SList(elems) =>
        elems.flatMap(collectFreeSymbols(_, patVars)).toSet
      case SchemeVal.SPair(cell) =>
        collectFreeSymbols(cell.car, patVars) ++ collectFreeSymbols(cell.cdr, patVars)
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
      case SchemeVal.SPair(cell) =>
        val newCar = instantiate(cell.car, bindings, defValues, gensymMap)
        val newCdr = instantiate(cell.cdr, bindings, defValues, gensymMap)
        SchemeVal.SPair(new MutableCell(newCar, newCdr))
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
      case SchemeVal.SPair(cell) =>
        findEllipsisVars(cell.car, bindings) ++ findEllipsisVars(cell.cdr, bindings)
      case _ => Set.empty
