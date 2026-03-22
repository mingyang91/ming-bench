package ming

import SchemeValue.*
import Evaluator.{EvalS, ReturnS, Step}

/** Derived special forms: letrec, let*, case, do, when. */
private[ming] object DerivedForms:

  def evalLetrec(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeList(bindings) :: body if body.nonEmpty =>
      val (names, inits) = parseBindings(bindings)
      val frame          = env.extend(names, names.map(_ => SchemeVoid))
      inits match
        case Nil => SpecialForms.startSequence(body, frame, k, out)
        case first :: rest =>
          EvalS(
            first,
            frame,
            Kont.LetrecInitK(names.head, names.tail, rest, body, frame, k),
            out
          )
    case _ => throw new EvalError("letrec: bad syntax")

  def evalLetStar(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeList(bindings) :: body if body.nonEmpty =>
      val parsed = bindings.map {
        case SchemeList(SchemeSymbol(name) :: init :: Nil) => (name, init)
        case other =>
          throw new EvalError(s"let*: bad binding: ${other.display}")
      }
      parsed match
        case Nil => SpecialForms.startSequence(body, env, k, out)
        case (name, init) :: rest =>
          EvalS(init, env, Kont.LetStarInitK(name, rest, body, env, k), out)
    case _ => throw new EvalError("let*: bad syntax")

  def evalCase(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case key :: clauses if clauses.nonEmpty =>
      EvalS(key, env, Kont.CaseK(clauses, env, k), out)
    case _ => throw new EvalError("case: bad syntax")

  def evalCaseClauses(
    key: SchemeValue,
    clauses: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = clauses match
    case Nil => ReturnS(SchemeVoid, k, out)
    case SchemeList(SchemeSymbol("else") :: body) :: _ =>
      SpecialForms.startSequence(body, env, k, out)
    case SchemeList(SchemeList(datums) :: body) :: rest =>
      if datums.exists(d => CollectionBuiltins.schemeEqv(key, d)) then SpecialForms.startSequence(body, env, k, out)
      else evalCaseClauses(key, rest, env, k, out)
    case other :: _ =>
      throw new EvalError(s"case: bad clause: ${other.display}")

  def evalDo(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeList(bindings) :: SchemeList(testClause) :: commands if testClause.nonEmpty =>
      val parsed = bindings.map {
        case SchemeList(SchemeSymbol(v) :: init :: step :: Nil) =>
          (v, init, step)
        case SchemeList(SchemeSymbol(v) :: init :: Nil) =>
          (v, init, SchemeSymbol(v))
        case other =>
          throw new EvalError(s"do: bad variable clause: ${other.display}")
      }
      buildDoLoop(parsed, testClause, commands, env, k, out)
    case _ => throw new EvalError("do: bad syntax")

  def evalWhen(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case test :: body if body.nonEmpty =>
      val ifExpr = SchemeList(
        SchemeSymbol("if") :: test :: SchemeList(
          SchemeSymbol("begin") :: body
        ) :: Nil
      )
      EvalS(ifExpr, env, k, out)
    case _ => throw new EvalError("when: bad syntax")

  // --- Helpers ---

  private def parseBindings(
    bindings: List[SchemeValue]
  ): (List[String], List[SchemeValue]) =
    bindings.map {
      case SchemeList(SchemeSymbol(name) :: init :: Nil) => (name, init)
      case other =>
        throw new EvalError(s"let: bad binding: ${other.display}")
    }.unzip

  private def buildDoLoop(
    parsed: List[(String, SchemeValue, SchemeValue)],
    testClause: List[SchemeValue],
    commands: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step =
    val test     = testClause.head
    val results  = testClause.tail
    val loopName = "__do"
    val letBinds = SchemeList(parsed.map { case (name, init, _) =>
      SchemeList(List(SchemeSymbol(name), init))
    })
    val steps    = parsed.map { case (_, _, step) => step }
    val loopCall = SchemeList(SchemeSymbol(loopName) :: steps)
    val testBranch =
      if results.nonEmpty then SchemeList(SchemeSymbol("begin") :: results)
      else SchemeList(List(SchemeSymbol("begin")))
    val elseBranch =
      if commands.nonEmpty then SchemeList(SchemeSymbol("begin") :: (commands :+ loopCall))
      else loopCall
    val ifExpr =
      SchemeList(List(SchemeSymbol("if"), test, testBranch, elseBranch))
    val namedLet = SchemeList(
      SchemeSymbol("let") :: SchemeSymbol(loopName) :: letBinds :: List(
        ifExpr
      )
    )
    EvalS(namedLet, env, k, out)
