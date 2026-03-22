package ming

import SchemeValue.*
import scala.annotation.tailrec

object Macros:
  type Env = Map[String, SchemeValue]

  sealed trait Binding
  case class SingleBinding(value: SchemeValue)          extends Binding
  case class RepeatedBinding(values: List[SchemeValue]) extends Binding

  type Bindings = Map[String, Binding]

  private val specialForms: Set[String] = Set(
    "if",
    "begin",
    "let",
    "set!",
    "quote",
    "define",
    "lambda",
    "cond",
    "and",
    "or",
    "define-syntax",
    "syntax-rules",
    "letrec"
  )

  private def freshName(base: String): String =
    s"${base}$$${java.util.UUID.randomUUID().toString.replace("-", "").take(8)}"

  def expand(
    literals: List[String],
    rules: List[(List[SchemeValue], SchemeValue)],
    input: List[SchemeValue],
    defEnv: Env
  ): (SchemeValue, Map[String, SchemeValue]) =
    val result = rules.iterator
      .flatMap { case (pattern, template) =>
        val patternArgs = pattern.tail
        matchPattern(patternArgs, input, literals).map { bindings =>
          expandWithHygiene(template, bindings, literals, defEnv)
        }
      }
      .nextOption()
    result.getOrElse(
      throw new EvalError("syntax-rules: no matching pattern")
    )

  private def matchPattern(
    pattern: List[SchemeValue],
    input: List[SchemeValue],
    literals: List[String]
  ): Option[Bindings] =
    val hasEllipsis = pattern.length >= 2 && (pattern.last match
      case SymbolVal("...", _) => true
      case _                   => false)
    if hasEllipsis then
      val fixedPattern = pattern.dropRight(2)
      val ellipsisVarName = pattern(pattern.length - 2) match
        case SymbolVal(name, _) => name
        case _                  => return None
      if input.length < fixedPattern.length then return None
      val (fixedInput, restInput) = input.splitAt(fixedPattern.length)
      matchElements(fixedPattern, fixedInput, literals).map { bindings =>
        bindings + (ellipsisVarName -> RepeatedBinding(restInput))
      }
    else if input.length != pattern.length then None
    else matchElements(pattern, input, literals)

  private def matchElements(
    pattern: List[SchemeValue],
    input: List[SchemeValue],
    literals: List[String]
  ): Option[Bindings] =
    pattern.zip(input).foldLeft(Option(Map.empty[String, Binding])) {
      case (None, _) => None
      case (Some(acc), (pat, inp)) =>
        matchSingle(pat, inp, literals).map(b => acc ++ b)
    }

  private def matchSingle(
    pat: SchemeValue,
    inp: SchemeValue,
    literals: List[String]
  ): Option[Bindings] =
    pat match
      case SymbolVal("_", _) => Some(Map.empty)
      case SymbolVal(name, _) if literals.contains(name) =>
        inp match
          case SymbolVal(n, _) if n == name => Some(Map.empty)
          case _                            => None
      case SymbolVal(name, _) =>
        Some(Map(name -> SingleBinding(inp)))
      case ListVal(patElems, _) =>
        inp match
          case ListVal(inpElems, _) => matchPattern(patElems, inpElems, literals)
          case _                    => None
      case _ =>
        if pat == inp then Some(Map.empty) else None

  private def expandWithHygiene(
    template: SchemeValue,
    bindings: Bindings,
    literals: List[String],
    defEnv: Env
  ): (SchemeValue, Map[String, SchemeValue]) =
    val patternVars = bindings.keySet
    val freeSyms    = collectFreeSymbols(template, patternVars, literals)
    val gensymMap: Map[String, String] = freeSyms.flatMap { name =>
      if defEnv.contains(name) then Some(name -> freshName(name))
      else None
    }.toMap
    val expanded = expandTemplate(template, bindings, gensymMap)
    val injected: Map[String, SchemeValue] = gensymMap.map { case (origName, gensymName) =>
      gensymName -> defEnv(origName)
    }
    (expanded, injected)

  private def collectFreeSymbols(
    template: SchemeValue,
    patternVars: Set[String],
    literals: List[String]
  ): Set[String] =
    template match
      case SymbolVal(name, _)
          if !patternVars.contains(name) &&
            !literals.contains(name) &&
            !specialForms.contains(name) &&
            name != "..." =>
        Set(name)
      case ListVal(elements, _) =>
        elements.flatMap(e => collectFreeSymbols(e, patternVars, literals)).toSet
      case _ => Set.empty

  private def expandTemplate(
    template: SchemeValue,
    bindings: Bindings,
    gensymMap: Map[String, String]
  ): SchemeValue =
    template match
      case SymbolVal(name, pos) =>
        bindings.get(name) match
          case Some(SingleBinding(value)) => value
          case Some(_: RepeatedBinding) =>
            throw new EvalError(
              s"syntax-rules: ellipsis variable $name used without ellipsis"
            )
          case None =>
            gensymMap.get(name) match
              case Some(newName) => SymbolVal(newName, pos)
              case None          => template
      case ListVal(elements, pos) =>
        ListVal(expandListElements(elements, bindings, gensymMap), pos)
      case _ => template

  private def expandListElements(
    elements: List[SchemeValue],
    bindings: Bindings,
    gensymMap: Map[String, String]
  ): List[SchemeValue] =
    elements match
      case Nil => Nil
      case elem :: SymbolVal("...", _) :: rest =>
        val repeatedVars = findRepeatedVars(elem, bindings)
        val expanded =
          if repeatedVars.isEmpty then Nil
          else
            val varName = repeatedVars.head
            bindings(varName) match
              case RepeatedBinding(values) =>
                values.map { v =>
                  expandTemplate(
                    elem,
                    bindings + (varName -> SingleBinding(v)),
                    gensymMap
                  )
                }
              case _ => Nil
        expanded ++ expandListElements(rest, bindings, gensymMap)
      case elem :: rest =>
        expandTemplate(elem, bindings, gensymMap) ::
          expandListElements(rest, bindings, gensymMap)

  private def findRepeatedVars(
    template: SchemeValue,
    bindings: Bindings
  ): List[String] =
    template match
      case SymbolVal(name, _) =>
        bindings.get(name) match
          case Some(_: RepeatedBinding) => List(name)
          case _                        => Nil
      case ListVal(elements, _) =>
        elements.flatMap(e => findRepeatedVars(e, bindings))
      case _ => Nil
