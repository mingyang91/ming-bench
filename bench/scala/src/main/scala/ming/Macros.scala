package ming

import scala.annotation.tailrec
import scala.collection.mutable

private[ming] final class MacroState:

  private val macros = mutable.LinkedHashMap.empty[String, SyntaxMacro]
  private var nextId = 0

  def define(name: String, macroDef: SyntaxMacro): Unit =
    macros.update(name, macroDef)

  def lookup(name: String): Option[SyntaxMacro] =
    macros.get(name)

  def isMacro(name: String): Boolean =
    macros.contains(name)

  def fresh(base: String): String =
    nextId += 1
    val suffix =
      base.map(ch => if ch.isLetterOrDigit then ch else '_').mkString match
        case ""   => "id"
        case text => text

    s"__macro_${nextId}_$suffix"

private[ming] object MacroState:

  def apply(): MacroState =
    new MacroState

private[ming] final case class SyntaxMacro(
  literals: Set[String],
  rules: List[SyntaxRule],
  definitionEnv: Env,
  aliases: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap.empty
)

private[ming] final case class SyntaxRule(pattern: Expr, template: Expr)

private[ming] object MacroExpander:

  private final case class MatchBindings(
    single: Map[String, Expr] = Map.empty,
    repeated: Map[String, Vector[Expr]] = Map.empty
  ):

    def merge(other: MatchBindings): Option[MatchBindings] =
      val mergedSingleOpt = other.single.foldLeft(Option(single)) {
        case (Some(acc), (name, expr)) if repeated.contains(name) =>
          None

        case (Some(acc), (name, expr)) =>
          acc.get(name) match
            case Some(existing) if !sameExpr(existing, expr) =>
              None

            case Some(_) =>
              Some(acc)

            case None =>
              Some(acc.updated(name, expr))

        case (None, _) =>
          None
      }

      mergedSingleOpt.flatMap { mergedSingle =>
        val mergedRepeatedOpt = other.repeated.foldLeft(Option(repeated)) {
          case (Some(acc), (name, values)) if mergedSingle.contains(name) =>
            None

          case (Some(acc), (name, values)) =>
            acc.get(name) match
              case Some(existing) if existing != values =>
                None

              case Some(_) =>
                Some(acc)

              case None =>
                Some(acc.updated(name, values))

          case (None, _) =>
            None
        }

        mergedRepeatedOpt.map(mergedRepeated => MatchBindings(mergedSingle, mergedRepeated))
      }

    def appendRepeated(other: MatchBindings): Option[MatchBindings] =
      if other.repeated.nonEmpty || other.single.keySet.exists(single.contains) then None
      else
        val mergedRepeated = other.single.foldLeft(repeated) { case (acc, (name, expr)) =>
          acc.updated(name, acc.getOrElse(name, Vector.empty) :+ expr)
        }

        Some(copy(repeated = mergedRepeated))

  private val syntaxKeywords = Set(
    "and",
    "begin",
    "cond",
    "define",
    "define-syntax",
    "else",
    "if",
    "lambda",
    "let",
    "or",
    "quote",
    "set!",
    "syntax-rules"
  )

  def define(args: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: transformer :: Nil =>
        macros.define(name, parseTransformer(transformer, env, pos))
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "define-syntax expects a name and transformer")

  def expand(expr: Expr, macros: MacroState): Expr =
    @tailrec
    def loop(current: Expr): Expr =
      current match
        case Expr.ListExpr(Expr.Symbol(name, _) :: _, pos) =>
          macros.lookup(name) match
            case Some(macroDef) =>
              loop(expandMacro(name, current, macroDef, macros, pos))

            case None =>
              current

        case _ =>
          current

    loop(expr)

  private def parseTransformer(expr: Expr, env: Env, pos: SourcePos): SyntaxMacro =
    expr match
      case Expr.ListExpr(Expr.Symbol("syntax-rules", _) :: Expr.ListExpr(literals, _) :: rules, transformerPos)
          if rules.nonEmpty =>
        val literalNames = literals.map {
          case Expr.Symbol(name, _) =>
            name

          case other =>
            throw EvalError.at(exprPos(other), "syntax-rules literal identifiers must be symbols")
        }.toSet

        val parsedRules = rules.map(parseRule)
        SyntaxMacro(literalNames, parsedRules, env)

      case _ =>
        throw EvalError.at(pos, "define-syntax expects a syntax-rules transformer")

  private def parseRule(expr: Expr): SyntaxRule =
    expr match
      case Expr.ListExpr(pattern :: template :: Nil, _) =>
        SyntaxRule(pattern, template)

      case Expr.ListExpr(_, pos) =>
        throw EvalError.at(pos, "syntax-rules clauses must contain exactly a pattern and template")

      case other =>
        throw EvalError.at(exprPos(other), "syntax-rules clauses must be lists")

  private def expandMacro(
    name: String,
    expr: Expr,
    macroDef: SyntaxMacro,
    macros: MacroState,
    pos: SourcePos
  ): Expr =
    macroDef.rules.iterator
      .map(rule => matchPattern(rule.pattern, expr, macroDef.literals + name).map(bindings =>
        instantiate(rule.template, bindings, macroDef, macros, Map.empty, None)
      ))
      .collectFirst { case Some(expanded) => expanded }
      .getOrElse(throw EvalError.at(pos, s"no syntax-rules clause matched: $name"))

  private def matchPattern(
    pattern: Expr,
    expr: Expr,
    literals: Set[String]
  ): Option[MatchBindings] =
    (pattern, expr) match
      case (Expr.IntAtom(left, _), Expr.IntAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.BoolAtom(left, _), Expr.BoolAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.StringAtom(left, _), Expr.StringAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.CharAtom(left, _), Expr.CharAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.Symbol(name, _), Expr.Symbol(other, _)) if literals.contains(name) && name == other =>
        Some(MatchBindings())

      case (Expr.Symbol(name, _), _) if !literals.contains(name) && name != "..." =>
        Some(MatchBindings(single = Map(name -> expr)))

      case (Expr.ListExpr(patternItems, _), Expr.ListExpr(exprItems, _)) =>
        matchListPattern(patternItems, exprItems, literals)

      case _ =>
        None

  private def matchListPattern(
    patterns: List[Expr],
    exprs: List[Expr],
    literals: Set[String]
  ): Option[MatchBindings] =
    def loop(
      remainingPatterns: List[Expr],
      remainingExprs: List[Expr],
      acc: MatchBindings
    ): Option[MatchBindings] =
      remainingPatterns match
        case Nil =>
          Option.when(remainingExprs.isEmpty)(acc)

        case pattern :: Expr.Symbol("...", _) :: rest =>
          val minTail = minimumPatternLength(rest)
          if remainingExprs.length < minTail then None
          else
            val maxRepeats = remainingExprs.length - minTail
            (maxRepeats to 0 by -1).iterator
              .map { repeatCount =>
                val (repeatedExprs, tailExprs) = remainingExprs.splitAt(repeatCount)
                for
                  repeatedBindings <- matchRepeatedPattern(pattern, repeatedExprs, literals)
                  merged           <- acc.merge(repeatedBindings)
                  matchedTail      <- loop(rest, tailExprs, merged)
                yield matchedTail
              }
              .collectFirst { case Some(bindings) => bindings }

        case pattern :: rest =>
          remainingExprs match
            case expr :: tail =>
              for
                matched <- matchPattern(pattern, expr, literals)
                merged  <- acc.merge(matched)
                result  <- loop(rest, tail, merged)
              yield result

            case Nil =>
              None

    loop(patterns, exprs, MatchBindings())

  private def matchRepeatedPattern(
    pattern: Expr,
    exprs: List[Expr],
    literals: Set[String]
  ): Option[MatchBindings] =
    val initialRepeated = repeatedPatternVariables(pattern, literals).map(_ -> Vector.empty[Expr]).toMap
    exprs.foldLeft(Option(MatchBindings(repeated = initialRepeated))) { (accOpt, expr) =>
      for
        acc     <- accOpt
        matched <- matchPattern(pattern, expr, literals)
        next    <- acc.appendRepeated(matched)
      yield next
    }

  private def minimumPatternLength(patterns: List[Expr]): Int =
    patterns match
      case Nil =>
        0

      case _ :: Expr.Symbol("...", _) :: rest =>
        minimumPatternLength(rest)

      case _ :: rest =>
        1 + minimumPatternLength(rest)

  private def instantiate(
    template: Expr,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    template match
      case Expr.Symbol(name, pos) =>
        instantiateSymbol(name, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(Expr.Symbol("quote", quotePos) :: quoted :: Nil, pos) =>
        Expr.ListExpr(List(Expr.Symbol("quote", quotePos), quoted), pos)

      case Expr.ListExpr(Expr.Symbol("lambda", lambdaPos) :: Expr.ListExpr(params, paramsPos) :: body, pos)
          if body.nonEmpty =>
        instantiateLambda(lambdaPos, params, paramsPos, body, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(Expr.Symbol("let", letPos) :: Expr.ListExpr(letBindings, bindingsPos) :: body, pos)
          if body.nonEmpty =>
        instantiateLet(letPos, letBindings, bindingsPos, body, pos, bindings, macroDef, macros, scope, repetitionIndex)

      case Expr.ListExpr(items, pos) =>
        Expr.ListExpr(instantiateItems(items, bindings, macroDef, macros, scope, repetitionIndex), pos)

      case other =>
        other

  private def instantiateLambda(
    lambdaPos: SourcePos,
    params: List[Expr],
    paramsPos: SourcePos,
    body: List[Expr],
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    val (expandedParams, paramScope) =
      instantiateBinderList(params, bindings, macroDef, macros, scope, repetitionIndex)

    val bodyScope   = scope ++ paramScope
    val expandedBody = body.map(expr => instantiate(expr, bindings, macroDef, macros, bodyScope, repetitionIndex))

    Expr.ListExpr(
      Expr.Symbol("lambda", lambdaPos) :: Expr.ListExpr(expandedParams, paramsPos) :: expandedBody,
      pos
    )

  private def instantiateLet(
    letPos: SourcePos,
    letBindings: List[Expr],
    bindingsPos: SourcePos,
    body: List[Expr],
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    val expandedBindings = List.newBuilder[Expr]
    var bodyScope        = scope

    letBindings.foreach {
      case Expr.ListExpr(nameExpr :: valueExpr :: Nil, bindingPos) =>
        val (expandedName, introduced) =
          instantiateBinder(nameExpr, bindings, macroDef, macros, scope, repetitionIndex)
        val expandedValue = instantiate(valueExpr, bindings, macroDef, macros, scope, repetitionIndex)

        bodyScope = bodyScope ++ introduced
        expandedBindings += Expr.ListExpr(List(expandedName, expandedValue), bindingPos)

      case Expr.ListExpr(_, bindingPos) =>
        throw EvalError.at(bindingPos, "macro let bindings must contain exactly a name and expression")

      case other =>
        throw EvalError.at(exprPos(other), "macro let bindings must be lists")
    }

    val expandedBody = body.map(expr => instantiate(expr, bindings, macroDef, macros, bodyScope, repetitionIndex))

    Expr.ListExpr(
      Expr.Symbol("let", letPos) :: Expr.ListExpr(expandedBindings.result(), bindingsPos) :: expandedBody,
      pos
    )

  private def instantiateBinderList(
    binders: List[Expr],
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): (List[Expr], Map[String, String]) =
    val expanded = List.newBuilder[Expr]
    var introduced = Map.empty[String, String]

    binders.foreach {
      case Expr.Symbol(".", pos) =>
        expanded += Expr.Symbol(".", pos)

      case binder =>
        val (expandedBinder, newScope) =
          instantiateBinder(binder, bindings, macroDef, macros, scope ++ introduced, repetitionIndex)
        expanded += expandedBinder
        introduced = introduced ++ newScope
    }

    (expanded.result(), introduced)

  private def instantiateBinder(
    binder: Expr,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): (Expr, Map[String, String]) =
    binder match
      case Expr.Symbol(name, pos) =>
        boundPatternExpr(name, pos, bindings, repetitionIndex) match
          case Some(Expr.Symbol(boundName, boundPos)) =>
            (Expr.Symbol(boundName, boundPos), Map.empty)

          case Some(other) =>
            throw EvalError.at(exprPos(other), "macro binder must expand to an identifier")

          case None =>
            val fresh = macros.fresh(name)
            (Expr.Symbol(fresh, pos), Map(name -> fresh))

      case other =>
        throw EvalError.at(exprPos(other), "macro binder must be an identifier")

  private def instantiateItems(
    items: List[Expr],
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): List[Expr] =
    val expanded = List.newBuilder[Expr]

    @tailrec
    def loop(remaining: List[Expr]): Unit =
      remaining match
        case template :: Expr.Symbol("...", _) :: rest =>
          val repeatCount = repeatedCount(template, bindings, scope.keySet, repetitionIndex)
          (0 until repeatCount).foreach { index =>
            expanded += instantiate(template, bindings, macroDef, macros, scope, Some(index))
          }
          loop(rest)

        case template :: rest =>
          expanded += instantiate(template, bindings, macroDef, macros, scope, repetitionIndex)
          loop(rest)

        case Nil =>
          ()

    loop(items)
    expanded.result()

  private def instantiateSymbol(
    name: String,
    pos: SourcePos,
    bindings: MatchBindings,
    macroDef: SyntaxMacro,
    macros: MacroState,
    scope: Map[String, String],
    repetitionIndex: Option[Int]
  ): Expr =
    boundPatternExpr(name, pos, bindings, repetitionIndex).getOrElse {
      scope.get(name) match
        case Some(freshName) =>
          Expr.Symbol(freshName, pos)

        case None if syntaxKeywords.contains(name) || macros.isMacro(name) =>
          Expr.Symbol(name, pos)

        case None =>
          Expr.Symbol(resolveAlias(name, macroDef, macros), pos)
    }

  private def boundPatternExpr(
    name: String,
    pos: SourcePos,
    bindings: MatchBindings,
    repetitionIndex: Option[Int]
  ): Option[Expr] =
    bindings.single.get(name).orElse {
      bindings.repeated.get(name).map { matches =>
        repetitionIndex match
          case Some(index) if index >= 0 && index < matches.length =>
            matches(index)

          case Some(index) =>
            throw EvalError.at(pos, s"macro repetition index out of bounds for: $name")

          case None =>
            throw EvalError.at(pos, s"repeated pattern variable used outside ellipsis: $name")
      }
    }

  private def resolveAlias(name: String, macroDef: SyntaxMacro, macros: MacroState): String =
    macroDef.aliases.getOrElseUpdate(
      name, {
        macroDef.definitionEnv.resolveCell(name) match
          case Some(cell) =>
            val alias = macros.fresh(name)
            macroDef.definitionEnv.defineAlias(alias, cell)
            alias

          case None =>
            macros.fresh(name)
      }
    )

  private def repeatedCount(
    template: Expr,
    bindings: MatchBindings,
    shadowedNames: Set[String],
    repetitionIndex: Option[Int]
  ): Int =
    val repeatedNames = collectRepeatedNames(template, bindings, shadowedNames, repetitionIndex)
    if repeatedNames.isEmpty then
      throw EvalError.at(exprPos(template), "ellipsis template must reference a repeated pattern variable")

    val lengths = repeatedNames.map(name => bindings.repeated(name).length)
    if lengths.size != 1 then
      throw EvalError.at(exprPos(template), "ellipsis template variables must repeat the same number of times")

    lengths.head

  private def collectRepeatedNames(
    template: Expr,
    bindings: MatchBindings,
    shadowedNames: Set[String],
    repetitionIndex: Option[Int]
  ): Set[String] =
    template match
      case Expr.Symbol(name, _) if !shadowedNames.contains(name) && bindings.repeated.contains(name) =>
        Set(name)

      case Expr.ListExpr(Expr.Symbol("quote", _) :: _ :: Nil, _) =>
        Set.empty

      case Expr.ListExpr(Expr.Symbol("lambda", _) :: Expr.ListExpr(params, _) :: body, _)
          if body.nonEmpty =>
        val introduced = binderNames(params, bindings, repetitionIndex)
        body.foldLeft(Set.empty[String]) { (acc, expr) =>
          acc ++ collectRepeatedNames(expr, bindings, shadowedNames ++ introduced, repetitionIndex)
        }

      case Expr.ListExpr(Expr.Symbol("let", _) :: Expr.ListExpr(letBindings, _) :: body, _)
          if body.nonEmpty =>
        val introduced = letBindings.flatMap {
          case Expr.ListExpr(nameExpr :: _ :: Nil, _) =>
            binderNames(List(nameExpr), bindings, repetitionIndex)

          case _ =>
            Nil
        }.toSet

        val bindingNames = letBindings.foldLeft(Set.empty[String]) {
          case (acc, Expr.ListExpr(_ :: valueExpr :: Nil, _)) =>
            acc ++ collectRepeatedNames(valueExpr, bindings, shadowedNames, repetitionIndex)

          case (acc, _) =>
            acc
        }

        bindingNames ++ body.foldLeft(Set.empty[String]) { (acc, expr) =>
          acc ++ collectRepeatedNames(expr, bindings, shadowedNames ++ introduced, repetitionIndex)
        }

      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String])((acc, item) => acc ++ collectRepeatedNames(item, bindings, shadowedNames, repetitionIndex))

      case _ =>
        Set.empty

  private def binderNames(
    binders: List[Expr],
    bindings: MatchBindings,
    repetitionIndex: Option[Int]
  ): List[String] =
    binders.flatMap {
      case Expr.Symbol(".", _) =>
        Nil

      case Expr.Symbol(name, pos) =>
        boundPatternExpr(name, pos, bindings, repetitionIndex) match
          case Some(_) =>
            Nil

          case None =>
            List(name)

      case _ =>
        Nil
    }

  private def repeatedPatternVariables(pattern: Expr, literals: Set[String]): Set[String] =
    pattern match
      case Expr.Symbol(name, _) if !literals.contains(name) && name != "..." =>
        Set(name)

      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String])((acc, item) => acc ++ repeatedPatternVariables(item, literals))

      case _ =>
        Set.empty

  private def sameExpr(left: Expr, right: Expr): Boolean =
    (left, right) match
      case (Expr.IntAtom(leftValue, _), Expr.IntAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.BoolAtom(leftValue, _), Expr.BoolAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.StringAtom(leftValue, _), Expr.StringAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.CharAtom(leftValue, _), Expr.CharAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.Symbol(leftName, _), Expr.Symbol(rightName, _)) =>
        leftName == rightName

      case (Expr.ListExpr(leftItems, _), Expr.ListExpr(rightItems, _)) =>
        leftItems.length == rightItems.length && leftItems.zip(rightItems).forall(sameExpr.tupled)

      case _ =>
        false

  private def exprPos(expr: Expr): SourcePos =
    expr match
      case Expr.IntAtom(_, pos)    => pos
      case Expr.BoolAtom(_, pos)   => pos
      case Expr.StringAtom(_, pos) => pos
      case Expr.CharAtom(_, pos)   => pos
      case Expr.Symbol(_, pos)     => pos
      case Expr.ListExpr(_, pos)   => pos
