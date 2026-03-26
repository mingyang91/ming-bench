package ming

import scala.annotation.tailrec

import SchemeModel.*
import SchemeMacros.*

private[ming] object SchemeMacroPatternMatcher:

  def matchPattern(
    pattern: Expr,
    input: Expr,
    bindings: PatternBindings,
    macroName: String,
    literals: Set[String],
    repeated: Boolean = false
  ): Option[PatternBindings] =
    pattern match
      case Expr.Symbol("_", _) =>
        Some(bindings)
      case Expr.Symbol(patternName, _) if isPatternVariable(patternName, macroName, literals) =>
        bindPatternVariable(patternName, input, bindings, repeated)
      case Expr.Symbol(patternName, _) =>
        input match
          case Expr.Symbol(inputName, _) if inputName == patternName =>
            Some(bindings)
          case _ =>
            None
      case Expr.IntegerLiteral(value, _) =>
        input match
          case Expr.IntegerLiteral(other, _) if other == value => Some(bindings)
          case _                                               => None
      case Expr.BooleanLiteral(value, _) =>
        input match
          case Expr.BooleanLiteral(other, _) if other == value => Some(bindings)
          case _                                               => None
      case Expr.StringLiteral(value, _) =>
        input match
          case Expr.StringLiteral(other, _) if other == value => Some(bindings)
          case _                                              => None
      case Expr.CharLiteral(value, _) =>
        input match
          case Expr.CharLiteral(other, _) if other == value => Some(bindings)
          case _                                            => None
      case Expr.ListExpr(patternItems, _) =>
        input match
          case Expr.ListExpr(inputItems, _) =>
            matchListPattern(patternItems, inputItems, bindings, macroName, literals)
          case _ =>
            None

  private def matchListPattern(
    patternItems: List[Expr],
    inputItems: List[Expr],
    bindings: PatternBindings,
    macroName: String,
    literals: Set[String]
  ): Option[PatternBindings] =
    patternItems match
      case Nil =>
        Option.when(inputItems.isEmpty)(bindings)
      case repeatedPattern :: Expr.Symbol("...", _) :: restPatterns =>
        matchRepeatedPattern(
          repeatedPattern,
          restPatterns,
          inputItems,
          bindings,
          macroName,
          literals
        )
      case headPattern :: tailPatterns =>
        inputItems match
          case headInput :: tailInput =>
            matchPattern(headPattern, headInput, bindings, macroName, literals).flatMap { nextBindings =>
              matchListPattern(tailPatterns, tailInput, nextBindings, macroName, literals)
            }
          case Nil =>
            None

  private def matchRepeatedPattern(
    repeatedPattern: Expr,
    restPatterns: List[Expr],
    inputItems: List[Expr],
    bindings: PatternBindings,
    macroName: String,
    literals: Set[String]
  ): Option[PatternBindings] =
    val maxRepetitions = inputItems.length

    @tailrec
    def loop(repetitions: Int): Option[PatternBindings] =
      if repetitions > maxRepetitions then None
      else
        val (repeatedInputs, remainingInputs) = inputItems.splitAt(repetitions)
        val afterRepeat = repeatedInputs.foldLeft(Option(bindings)) { (current, input) =>
          current.flatMap { nextBindings =>
            matchPattern(
              repeatedPattern,
              input,
              nextBindings,
              macroName,
              literals,
              repeated = true
            )
          }
        }

        afterRepeat
          .flatMap(nextBindings =>
            matchListPattern(restPatterns, remainingInputs, nextBindings, macroName, literals)
          ) match
          case some @ Some(_) => some
          case None           => loop(repetitions + 1)

    loop(0)

  private def bindPatternVariable(
    name: String,
    input: Expr,
    bindings: PatternBindings,
    repeated: Boolean
  ): Option[PatternBindings] =
    bindings.get(name) match
      case None =>
        Some(bindings.updated(name, Vector(input)))
      case Some(existing) if repeated =>
        Some(bindings.updated(name, existing :+ input))
      case Some(existing) if existing.length == 1 && exprEquals(existing.head, input) =>
        Some(bindings)
      case _ =>
        None

  private def exprEquals(left: Expr, right: Expr): Boolean =
    (left, right) match
      case (Expr.IntegerLiteral(a, _), Expr.IntegerLiteral(b, _)) => a == b
      case (Expr.BooleanLiteral(a, _), Expr.BooleanLiteral(b, _)) => a == b
      case (Expr.StringLiteral(a, _), Expr.StringLiteral(b, _))   => a == b
      case (Expr.CharLiteral(a, _), Expr.CharLiteral(b, _))       => a == b
      case (Expr.Symbol(a, _), Expr.Symbol(b, _))                 => a == b
      case (Expr.ListExpr(as, _), Expr.ListExpr(bs, _)) =>
        as.length == bs.length && as.zip(bs).forall(exprEquals.tupled)
      case _ =>
        false
