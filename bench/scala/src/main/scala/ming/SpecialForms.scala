package ming

import SchemeValue.*
import Evaluator.{EvalS, ReturnS, Step}

/** Special form evaluation and sequence helpers. */
private[ming] object SpecialForms:

  private type Define =
    (String, Option[(List[String], Option[String])], List[SchemeValue])

  // --- Sequence / body helpers ---

  def startSequence(
    exprs: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step =
    val (defines, body) = collectDefines(exprs, Nil)
    if defines.isEmpty then startBody(body, env, k, out)
    else
      val frame = new Env.LetrecFrame(defines, env)
      frame.initFunctions()
      val varInits = defines.collect { case (name, None, List(valueExpr)) =>
        SchemeList(List(SchemeSymbol("set!"), SchemeSymbol(name), valueExpr))
      }
      startBody(varInits ++ body, frame, k, out)

  def startBody(
    body: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = body match
    case Nil           => ReturnS(SchemeVoid, k, out)
    case last :: Nil   => EvalS(last, env, k, out)
    case first :: rest => EvalS(first, env, Kont.Seq(rest, env, k), out)

  // --- Special form dispatch ---

  def evalSpecial(
    op: String,
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = op match
    case "define" => evalDefine(args, env, k, out)
    case "if"     => evalIf(args, env, k, out)
    case "quote" =>
      if args.length != 1 then throw new EvalError("quote: expected 1 argument")
      ReturnS(args.head, k, out)
    case "lambda" => ReturnS(makeLambda(args, env), k, out)
    case "and"    => evalAnd(args, env, k, out)
    case "or"     => evalOr(args, env, k, out)
    case "not" =>
      if args.length != 1 then throw new EvalError("not: expected 1 argument")
      EvalS(args.head, env, Kont.NotK(k), out)
    case "let"   => evalLet(args, env, k, out)
    case "begin" => startSequence(args, env, k, out)
    case "cond"  => evalCondClauses(args, env, k, out)
    case "set!"  => evalSet(args, env, k, out)
    case "call/cc" | "call-with-current-continuation" =>
      if args.length != 1 then throw new EvalError("call/cc: expected 1 argument")
      EvalS(args.head, env, Kont.CallCCK(k), out)
    case "define-syntax" => evalDefineSyntax(args, env, k, out)
    case _               => throw new EvalError(s"unknown special form: $op")

  // --- Special form implementations ---

  private def evalDefine(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeSymbol(name) :: valueExpr :: Nil =>
      EvalS(valueExpr, env, Kont.DefineK(name, env, k), out)
    case SchemeList(SchemeSymbol(name) :: rawParams) :: body =>
      val (paramNames, restParam) = parseParams(rawParams)
      val recEnv = Env.RecursiveFrame(
        name,
        closure => SchemeLambda(paramNames, restParam, body, closure),
        env
      )
      val updatedK = Evaluator.updateSeqEnv(k, recEnv)
      ReturnS(SchemeVoid, updatedK, out)
    case _ => throw new EvalError("bad define syntax")

  private def evalIf(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case cond :: thenE :: elseE :: Nil =>
      EvalS(cond, env, Kont.IfK(thenE, Some(elseE), env, k), out)
    case cond :: thenE :: Nil =>
      EvalS(cond, env, Kont.IfK(thenE, None, env, k), out)
    case _ => throw new EvalError("if: bad syntax")

  private def evalAnd(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case Nil          => ReturnS(SchemeBool(true), k, out)
    case last :: Nil  => EvalS(last, env, k, out)
    case head :: tail => EvalS(head, env, Kont.AndK(tail, env, k), out)

  private def evalOr(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case Nil          => ReturnS(SchemeBool(false), k, out)
    case last :: Nil  => EvalS(last, env, k, out)
    case head :: tail => EvalS(head, env, Kont.OrK(tail, env, k), out)

  private def evalSet(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeSymbol(name) :: valueExpr :: Nil =>
      EvalS(valueExpr, env, Kont.SetK(name, env, k), out)
    case _ => throw new EvalError("set!: bad syntax")

  private def evalLet(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeSymbol(name) :: SchemeList(bindings) :: body if body.nonEmpty =>
      val (params, inits) = parseBindings(bindings)
      inits match
        case Nil =>
          val recEnv = Env.RecursiveFrame(
            name,
            closure => SchemeLambda(params, None, body, closure),
            env
          )
          startSequence(body, recEnv.extend(params, Nil), k, out)
        case first :: rest =>
          EvalS(
            first,
            env,
            Kont.NamedLetInitK(name, params, Nil, rest, body, env, k),
            out
          )
    case SchemeList(bindings) :: body if body.nonEmpty =>
      val (params, inits) = parseBindings(bindings)
      inits match
        case Nil =>
          startSequence(body, env.extend(params, Nil), k, out)
        case first :: rest =>
          EvalS(
            first,
            env,
            Kont.LetInitK(params, Nil, rest, body, env, k),
            out
          )
    case _ => throw new EvalError("let: bad syntax")

  def evalCondClauses(
    clauses: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = clauses match
    case Nil => ReturnS(SchemeVoid, k, out)
    case SchemeList(SchemeSymbol("else") :: body) :: _ =>
      startSequence(body, env, k, out)
    case SchemeList(test :: body) :: rest =>
      EvalS(test, env, Kont.CondK(body, rest, env, k), out)
    case other :: _ =>
      throw new EvalError(s"cond: bad clause: ${other.display}")

  private def evalDefineSyntax(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step = args match
    case SchemeSymbol(name) :: syntaxForm :: Nil =>
      val m        = Macros.parseSyntaxRules(syntaxForm, env)
      val newEnv   = env.extend(name, m)
      val updatedK = Evaluator.updateSeqEnv(k, newEnv)
      ReturnS(SchemeVoid, updatedK, out)
    case _ => throw new EvalError("bad define-syntax syntax")

  // --- Parsing helpers ---

  private[ming] def parseParams(
    rawParams: List[SchemeValue]
  ): (List[String], Option[String]) =
    val dotIdx = rawParams.indexWhere {
      case SchemeSymbol(".") => true
      case _                 => false
    }
    if dotIdx < 0 then
      val names = rawParams.map {
        case SchemeSymbol(n) => n
        case other           => throw new EvalError(s"bad parameter: ${other.display}")
      }
      (names, None)
    else
      val fixed = rawParams.take(dotIdx).map {
        case SchemeSymbol(n) => n
        case other           => throw new EvalError(s"bad parameter: ${other.display}")
      }
      rawParams.drop(dotIdx + 1) match
        case SchemeSymbol(rest) :: Nil => (fixed, Some(rest))
        case _                         => throw new EvalError("bad dot syntax in parameters")

  @scala.annotation.tailrec
  def collectDefines(
    exprs: List[SchemeValue],
    acc: List[Define]
  ): (List[Define], List[SchemeValue]) =
    exprs match
      case (defExpr @ SchemeList(
            SchemeSymbol("define") :: rest
          )) :: tail =>
        rest match
          case SchemeSymbol(name) :: valueExpr :: Nil =>
            collectDefines(tail, (name, None, List(valueExpr)) :: acc)
          case SchemeList(SchemeSymbol(name) :: rawParams) :: body =>
            val (paramNames, restParam) = parseParams(rawParams)
            collectDefines(
              tail,
              (name, Some((paramNames, restParam)), body) :: acc
            )
          case _ =>
            throw new EvalError("bad define syntax", defExpr.pos)
      case _ => (acc.reverse, exprs)

  private def parseBindings(
    bindings: List[SchemeValue]
  ): (List[String], List[SchemeValue]) =
    bindings.map {
      case SchemeList(SchemeSymbol(name) :: init :: Nil) => (name, init)
      case other =>
        throw new EvalError(s"let: bad binding: ${other.display}")
    }.unzip

  private def makeLambda(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue = args match
    case SchemeList(rawParams) :: body if body.nonEmpty =>
      val (paramNames, restParam) = parseParams(rawParams)
      SchemeLambda(paramNames, restParam, body, env)
    case SchemeSymbol(restOnly) :: body if body.nonEmpty =>
      SchemeLambda(Nil, Some(restOnly), body, env)
    case _ => throw new EvalError("lambda: bad syntax")
