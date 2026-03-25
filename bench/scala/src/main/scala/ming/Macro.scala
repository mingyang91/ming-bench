package ming

import scala.annotation.tailrec

object Macro:

  private val gensymCounter: java.util.concurrent.atomic.AtomicLong =
    java.util.concurrent.atomic.AtomicLong(0)

  private def gensym(prefix: String): String =
    val n = gensymCounter.incrementAndGet()
    s"${prefix}__g${n}"

  sealed trait PatternBinding
  case class SingleBinding(value: SchemeVal)          extends PatternBinding
  case class EllipsisBinding(values: List[SchemeVal]) extends PatternBinding

  type Bindings = Map[String, PatternBinding]

  private val specialForms: Set[String] = Set(
    "define",
    "if",
    "quote",
    "lambda",
    "and",
    "or",
    "begin",
    "let",
    "cond",
    "set!",
    "define-syntax",
    "syntax-rules",
    "let*",
    "letrec",
    "do",
    "case",
    "quasiquote",
    "unquote",
    "unquote-splicing"
  )

  def expand(
    macroName: String,
    literals: List[String],
    rules: List[(SchemeVal, SchemeVal)],
    defEnv: Env,
    inputForm: SchemeVal
  ): SchemeVal =
    val input = inputForm match
      case SchemeVal.SList(elems) => elems
      case _                      => throw new EvalError(s"bad macro application: $inputForm")

    rules
      .collectFirst {
        case (pattern, template) if matchRule(pattern, input, literals).isDefined =>
          val bindings    = matchRule(pattern, input, literals).get
          val patternVars = bindings.keySet
          val freeVars    = collectSymbols(template) -- patternVars -- specialForms - macroName
          val gensymMap   = freeVars.map(v => v -> gensym(v)).toMap
          val expanded    = expandTemplate(template, bindings, gensymMap)
          val defBindings = gensymMap.flatMap { case (orig, fresh) =>
            defEnv.lookup(orig).map(v => (fresh, v))
          }.toList
          if defBindings.nonEmpty then
            val letBindings = defBindings.map { case (name, value) =>
              SchemeVal.SList(List(SchemeVal.Symbol(name), value))
            }
            SchemeVal.SList(
              List(
                SchemeVal.Symbol("let"),
                SchemeVal.SList(letBindings),
                expanded
              )
            )
          else expanded
      }
      .getOrElse(throw new EvalError(s"no matching pattern for macro $macroName"))

  private def matchRule(
    pattern: SchemeVal,
    input: List[SchemeVal],
    literals: List[String]
  ): Option[Bindings] =
    pattern match
      case SchemeVal.SList(patElems) if patElems.nonEmpty =>
        matchElements(patElems.tail, input.tail, literals)
      case _ => None

  private def matchElements(
    patterns: List[SchemeVal],
    inputs: List[SchemeVal],
    literals: List[String]
  ): Option[Bindings] =
    val ellipsisIdx = patterns.indexWhere {
      case SchemeVal.Symbol("...") => true
      case _                       => false
    }

    if ellipsisIdx > 0 then matchWithEllipsis(patterns, inputs, literals, ellipsisIdx)
    else matchWithoutEllipsis(patterns, inputs, literals)

  private def matchWithEllipsis(
    patterns: List[SchemeVal],
    inputs: List[SchemeVal],
    literals: List[String],
    ellipsisIdx: Int
  ): Option[Bindings] =
    val fixedPatterns   = patterns.take(ellipsisIdx - 1)
    val ellipsisPattern = patterns(ellipsisIdx - 1)
    val afterEllipsis   = patterns.drop(ellipsisIdx + 1)

    if inputs.size < fixedPatterns.size + afterEllipsis.size then None
    else
      for
        fixedBindings <- foldMatchBindings(fixedPatterns.zip(inputs), literals)
        ellipsisBindings <- ellipsisPattern match
          case SchemeVal.Symbol(name) if !literals.contains(name) =>
            val ellipsisInputs = inputs.drop(fixedPatterns.size).dropRight(afterEllipsis.size)
            Some(Map(name -> EllipsisBinding(ellipsisInputs)))
          case _ => None
        afterInputs = inputs.takeRight(afterEllipsis.size)
        afterBindings <- foldMatchBindings(afterEllipsis.zip(afterInputs), literals)
      yield fixedBindings ++ ellipsisBindings ++ afterBindings

  private def matchWithoutEllipsis(
    patterns: List[SchemeVal],
    inputs: List[SchemeVal],
    literals: List[String]
  ): Option[Bindings] =
    if patterns.size != inputs.size then None
    else foldMatchBindings(patterns.zip(inputs), literals)

  private def foldMatchBindings(
    pairs: List[(SchemeVal, SchemeVal)],
    literals: List[String]
  ): Option[Bindings] =
    pairs.foldLeft(Option(Map.empty: Bindings)) { case (acc, (pat, inp)) =>
      acc.flatMap(bindings => matchSingle(pat, inp, literals).map(bindings ++ _))
    }

  private def matchSingle(
    pattern: SchemeVal,
    input: SchemeVal,
    literals: List[String]
  ): Option[Bindings] =
    pattern match
      case SchemeVal.Symbol(name) if literals.contains(name) =>
        input match
          case SchemeVal.Symbol(n) if n == name => Some(Map.empty)
          case _                                => None
      case SchemeVal.Symbol("_") =>
        Some(Map.empty)
      case SchemeVal.Symbol(name) =>
        Some(Map(name -> SingleBinding(input)))
      case SchemeVal.SList(subPats) =>
        input match
          case SchemeVal.SList(subInputs) => matchElements(subPats, subInputs, literals)
          case _                          => None
      case _ =>
        if SchemeVal.schemeEqual(pattern, input) then Some(Map.empty) else None

  private def collectSymbols(template: SchemeVal): Set[String] =
    template match
      case SchemeVal.Symbol(name) if name != "..." => Set(name)
      case SchemeVal.SList(elems)                  => elems.flatMap(collectSymbols).toSet
      case _                                       => Set.empty

  private def expandTemplate(
    template: SchemeVal,
    bindings: Bindings,
    gensymMap: Map[String, String]
  ): SchemeVal =
    template match
      case SchemeVal.Symbol(name) =>
        bindings.get(name) match
          case Some(SingleBinding(v)) => v
          case Some(EllipsisBinding(_)) =>
            throw new EvalError(s"ellipsis variable $name used without ellipsis")
          case None =>
            gensymMap.get(name) match
              case Some(fresh) => SchemeVal.Symbol(fresh)
              case None        => template
      case SchemeVal.SList(elems) =>
        elems match
          case SchemeVal.Symbol("quote") :: _ => template
          case _                              => SchemeVal.SList(expandListTemplate(elems, bindings, gensymMap))
      case _ => template

  @tailrec
  private def expandListTemplate(
    elems: List[SchemeVal],
    bindings: Bindings,
    gensymMap: Map[String, String],
    acc: List[SchemeVal] = Nil
  ): List[SchemeVal] =
    elems match
      case Nil => acc.reverse
      case sub :: SchemeVal.Symbol("...") :: rest =>
        val ellipsisVars = findEllipsisVars(sub, bindings)
        val expanded =
          if ellipsisVars.nonEmpty then
            val count = bindings(ellipsisVars.head) match
              case EllipsisBinding(vs) => vs.size
              case _                   => 0
            (0 until count).toList.map { j =>
              val iterBindings = bindings.map {
                case (k, EllipsisBinding(vs)) if ellipsisVars.contains(k) =>
                  k -> SingleBinding(vs(j))
                case other => other
              }
              expandTemplate(sub, iterBindings, gensymMap)
            }
          else Nil
        expandListTemplate(rest, bindings, gensymMap, expanded.reverse ++ acc)
      case head :: rest =>
        expandListTemplate(rest, bindings, gensymMap, expandTemplate(head, bindings, gensymMap) :: acc)

  private def findEllipsisVars(template: SchemeVal, bindings: Bindings): Set[String] =
    template match
      case SchemeVal.Symbol(name) =>
        bindings.get(name) match
          case Some(EllipsisBinding(_)) => Set(name)
          case _                        => Set.empty
      case SchemeVal.SList(elems) => elems.flatMap(e => findEllipsisVars(e, bindings)).toSet
      case _                      => Set.empty
