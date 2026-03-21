package ming

import scala.annotation.tailrec

object Eval:

  private[ming] enum Bounce:
    case Done(value: Value, output: String)
    case More(expr: Value, env: Env, output: String)
    case Guard(exnVar: String, clauses: List[Value], body: List[Value], env: Env, output: String)

  private[ming] def prependOutput(b: Bounce, prefix: String): Bounce =
    if prefix.isEmpty then b
    else
      b match
        case Bounce.Done(v, o)               => Bounce.Done(v, prefix + o)
        case Bounce.More(e, env, o)          => Bounce.More(e, env, prefix + o)
        case Bounce.Guard(ev, cl, bd, ge, o) => Bounce.Guard(ev, cl, bd, ge, prefix + o)

  def eval(expr: Value, env: Env, accOut: String = ""): (Value, String) =
    resolveToValue(evalBounce(expr, env), accOut)

  private[ming] def resolveToValue(initial: Bounce, accOut: String): (Value, String) =
    @tailrec def loop(b: Bounce, acc: String): (Value, String) = b match
      case Bounce.Done(v, o)       => (v, acc + o)
      case Bounce.More(e, env2, o) => loop(evalBounce(e, env2), acc + o)
      case g: Bounce.Guard =>
        val (v, o) = EvalGuard.evalGuardFull(g.exnVar, g.clauses, g.body, g.env)
        (v, acc + g.output + o)
    loop(initial, accOut)

  def evalTopLevel(expr: Value, env: Env): (Value, Env, String) = expr match
    case Value.SList(Value.Symbol("define") :: rest)             => evalDefine(rest, env)
    case Value.SList(Value.Symbol("define-syntax") :: rest)      => Macro.evalDefineSyntax(rest, env)
    case Value.SList(Value.Symbol("define-record-type") :: rest) => evalDefineRecordType(rest, env)
    case Value.SList(elems @ (Value.Symbol(name) :: _)) =>
      Macro.tryApply(name, elems, env) match
        case Some((expanded, localEnv)) => evalTopLevel(expanded, localEnv)
        case None =>
          val (v, out) = eval(expr, env)
          (v, env, out)
    case _ =>
      val (v, out) = eval(expr, env)
      (v, env, out)

  private[ming] def evalBounce(expr: Value, env: Env): Bounce = expr match
    case Value.Integer(_) | Value.Bool(_) | Value.Str(_) | Value.MutStr(_) | Value.Char(_) | Value.Rational(_, _) |
        Value.Float(_) =>
      Bounce.Done(expr, "")
    case Value.Symbol(name)         => Bounce.Done(env.lookup(name), "")
    case Value.SList(elems)         => evalListBounce(elems, env)
    case _: Value.Lambda            => Bounce.Done(expr, "")
    case _: Value.Continuation      => Bounce.Done(expr, "")
    case _: Value.Macro             => Bounce.Done(expr, "")
    case _: Value.TransformerMacro  => Bounce.Done(expr, "")
    case _: Value.Vec               => Bounce.Done(expr, "")
    case _: Value.Pair              => Bounce.Done(expr, "")
    case Value.Values(_)            => Bounce.Done(expr, "")
    case _: Value.Record            => Bounce.Done(expr, "")
    case _: Value.RecordConstructor => Bounce.Done(expr, "")
    case _: Value.RecordPredicate   => Bounce.Done(expr, "")
    case _: Value.RecordAccessor    => Bounce.Done(expr, "")
    case Value.Void                 => Bounce.Done(Value.Void, "")

  private def evalListBounce(elems: List[Value], env: Env): Bounce = elems match
    case Nil                                 => throw new EvalError("empty application")
    case Value.Symbol("quote") :: args       => Bounce.Done(evalQuote(args), "")
    case Value.Symbol("if") :: args          => evalIfBounce(args, env)
    case Value.Symbol("lambda") :: args      => Bounce.Done(evalLambda(args, env), "")
    case Value.Symbol("define") :: _         => throw new EvalError("define not at top level")
    case Value.Symbol("set!") :: args        => evalSetBounce(args, env)
    case Value.Symbol("let") :: args         => evalLetBounce(args, env)
    case Value.Symbol("begin") :: body       => evalBodyBounce(body, env, "")
    case Value.Symbol("cond") :: clauses     => evalCondBounce(clauses, env, "")
    case Value.Symbol("and") :: args         => evalAndBounce(args, Value.Bool(true), env, "")
    case Value.Symbol("letrec") :: args      => EvalLetrec.evalLetrecBounce(args, env)
    case Value.Symbol("letrec*") :: args     => EvalLetrec.evalLetrecStarBounce(args, env)
    case Value.Symbol("case") :: args        => EvalGuard.evalCaseBounce(args, env)
    case Value.Symbol("or") :: args          => evalOrBounce(args, Value.Bool(false), env, "")
    case Value.Symbol("guard") :: args       => EvalGuard.evalGuardBounce(args, env)
    case Value.Symbol("syntax-case") :: args => SyntaxCase.evalSyntaxCaseBounce(args, env)
    case Value.Symbol("syntax") :: args      => SyntaxCase.evalSyntaxBounce(args, env)
    case Value.Symbol("with-syntax") :: args => SyntaxCase.evalWithSyntaxBounce(args, env)
    case Value.Symbol(name) :: args =>
      Macro.tryApply(name, elems, env) match
        case Some((expanded, localEnv)) => Bounce.More(expanded, localEnv, "")
        case None                       => evalApplication(Value.Symbol(name), args, env)
    case head :: args => evalApplication(head, args, env)

  private def evalApplication(head: Value, args: List[Value], env: Env): Bounce =
    val (op, out1)         = eval(head, env)
    val (evaledArgs, out2) = evalArgList(args, env)
    prependOutput(EvalApply.applyFnBounce(op, evaledArgs), out1 + out2)

  private def evalQuote(args: List[Value]): Value = args match
    case single :: Nil => single
    case _             => throw new EvalError("quote requires exactly 1 argument")

  private def evalIfBounce(args: List[Value], env: Env): Bounce = args match
    case cond :: thenBr :: elseBr :: Nil =>
      val (test, o1) = eval(cond, env)
      Bounce.More(if Value.isFalsy(test) then elseBr else thenBr, env, o1)
    case cond :: thenBr :: Nil =>
      val (test, o1) = eval(cond, env)
      if Value.isFalsy(test) then Bounce.Done(Value.Void, o1)
      else Bounce.More(thenBr, env, o1)
    case _ => throw new EvalError("if requires 2 or 3 arguments")

  private def evalLambda(args: List[Value], env: Env): Value = args match
    case Value.SList(params) :: body if body.nonEmpty =>
      val (fixed, rest) = splitParams(params)
      Value.Lambda(fixed, body, env, restParam = rest)
    case Value.Symbol(name) :: body if body.nonEmpty =>
      Value.Lambda(Nil, body, env, restParam = Some(name))
    case _ => throw new EvalError("bad lambda syntax")

  private def splitParams(params: List[Value]): (List[String], Option[String]) =
    val dotIdx = params.indexWhere(_ == Value.Symbol("."))
    if dotIdx < 0 then
      val names = params.map {
        case Value.Symbol(p) => p
        case other           => throw new EvalError(s"expected parameter name, got: ${other.display}")
      }
      (names, None)
    else
      val fixed = params.take(dotIdx).map {
        case Value.Symbol(p) => p
        case other           => throw new EvalError(s"expected parameter name, got: ${other.display}")
      }
      params.drop(dotIdx + 1) match
        case Value.Symbol(rest) :: Nil => (fixed, Some(rest))
        case _                         => throw new EvalError("bad dot syntax in parameter list")

  private def evalLetBounce(args: List[Value], env: Env): Bounce = args match
    case Value.Symbol(name) :: Value.SList(bindings) :: body if body.nonEmpty =>
      val (pairs, o1) = evalBindings(bindings, env)
      val paramNames  = pairs.map(_._1)
      val initValues  = pairs.map(_._2)
      val lambda      = Value.Lambda(paramNames, body, env, Some(name))
      prependOutput(EvalApply.applyFnBounce(lambda, initValues), o1)
    case Value.SList(bindings) :: body if body.nonEmpty =>
      val (pairs, o1) = evalBindings(bindings, env)
      val localEnv    = env.extendAll(pairs.map(_._1), pairs.map(_._2))
      evalBodyBounce(body, localEnv, o1)
    case _ => throw new EvalError("bad let syntax")

  private def evalBindings(bindings: List[Value], env: Env): (List[(String, Value)], String) =
    bindings.foldLeft((List.empty[(String, Value)], "")) { case ((pairs, out), b) =>
      b match
        case Value.SList(Value.Symbol(name) :: expr :: Nil) =>
          val (v, o) = eval(expr, env)
          (pairs :+ (name, v), out + o)
        case other => throw new EvalError(s"bad let binding: ${other.display}")
    }

  @tailrec
  private def evalCondBounce(clauses: List[Value], env: Env, out: String): Bounce =
    clauses match
      case Nil => Bounce.Done(Value.Void, out)
      case Value.SList(Value.Symbol("else") :: body) :: _ =>
        evalBodyBounce(body, env, out)
      case Value.SList(test :: body) :: rest =>
        val (result, o) = eval(test, env)
        if !Value.isFalsy(result) then evalBodyBounce(body, env, out + o)
        else evalCondBounce(rest, env, out + o)
      case other :: _ => throw new EvalError(s"bad cond clause: ${other.display}")

  @tailrec
  private[ming] def evalBodyBounce(body: List[Value], env: Env, acc: String): Bounce =
    body match
      case Nil => Bounce.Done(Value.Void, acc)
      case last :: Nil =>
        last match
          case Value.SList(Value.Symbol("define") :: rest) =>
            val (v, _, o) = evalDefine(rest, env)
            Bounce.Done(v, acc + o)
          case Value.SList(Value.Symbol("define-record-type") :: rest) =>
            val (v, _, o) = evalDefineRecordType(rest, env)
            Bounce.Done(v, acc + o)
          case _ => Bounce.More(last, env, acc)
      case head :: tail =>
        val (newEnv, o) = head match
          case Value.SList(Value.Symbol("define") :: rest) =>
            val (_, e, o2) = evalDefine(rest, env)
            (e, o2)
          case Value.SList(Value.Symbol("define-record-type") :: rest) =>
            val (_, e, o2) = evalDefineRecordType(rest, env)
            (e, o2)
          case Value.SList(Value.Symbol("define-syntax") :: rest) =>
            val (_, e, o2) = Macro.evalDefineSyntax(rest, env)
            (e, o2)
          case _ =>
            val (_, o2) = eval(head, env)
            (env, o2)
        evalBodyBounce(tail, newEnv, acc + o)

  private def evalDefineRecordType(args: List[Value], env: Env): (Value, Env, String) =
    args match
      case Value.Symbol(typeName) :: Value.SList(Value.Symbol(ctorName) :: ctorFields) :: Value.Symbol(
            predName
          ) :: fieldSpecs =>
        val fieldNames = ctorFields.map {
          case Value.Symbol(n) => n
          case other           => throw new EvalError(s"bad field name in constructor: ${other.display}")
        }
        val tag         = typeName
        val constructor = Value.RecordConstructor(tag, fieldNames.length)
        val predicate   = Value.RecordPredicate(tag)
        val accessors = fieldSpecs.map {
          case Value.SList(Value.Symbol(fname) :: Value.Symbol(accName) :: Nil) =>
            val idx = fieldNames.indexOf(fname)
            if idx < 0 then throw new EvalError(s"field $fname not in constructor")
            (accName, Value.RecordAccessor(tag, idx))
          case other => throw new EvalError(s"bad field spec: ${other.display}")
        }
        val allBindings = (ctorName, constructor) :: (predName, predicate) :: accessors
        val newEnv = allBindings.foldLeft(env) { case (e, (name, value)) =>
          updateSharedCell(e, name, value)
          e.extend(name, value)
        }
        (Value.Void, newEnv, "")
      case _ => throw new EvalError("bad define-record-type syntax")

  private def evalDefine(args: List[Value], env: Env): (Value, Env, String) = args match
    case Value.Symbol(name) :: body :: Nil =>
      val (value, out) = eval(body, env)
      updateSharedCell(env, name, value)
      (Value.Void, env.extend(name, value), out)
    case Value.SList(Value.Symbol(name) :: params) :: body =>
      val (fixed, rest) = splitParams(params)
      val lambda        = Value.Lambda(fixed, body, env, Some(name), rest)
      updateSharedCell(env, name, lambda)
      (Value.Void, env.extend(name, lambda), "")
    case _ => throw new EvalError("bad define syntax")

  private def evalSetBounce(args: List[Value], env: Env): Bounce = args match
    case Value.Symbol(name) :: expr :: Nil =>
      val (value, out) = eval(expr, env)
      env.set(name, value)
      Bounce.Done(Value.Void, out)
    case _ => throw new EvalError("bad set! syntax")

  @tailrec
  def updateSharedCell(env: Env, name: String, value: Value): Unit =
    env.shared match
      case Some(cell) => cell(0) = cell(0) + (name -> value)
      case None =>
        env.parent match
          case Some(p) => updateSharedCell(p, name, value)
          case None    => ()

  private[ming] def resolveBounce(b: Bounce): (Value, String) = resolveToValue(b, "")

  private def evalArgList(args: List[Value], env: Env): (List[Value], String) =
    args.foldLeft((List.empty[Value], "")) { case ((vals, out), arg) =>
      val (v, o) = eval(arg, env)
      (vals :+ v, out + o)
    }

  @tailrec
  private def evalAndBounce(
    args: List[Value],
    last: Value,
    env: Env,
    out: String
  ): Bounce = args match
    case Nil         => Bounce.Done(last, out)
    case head :: Nil => Bounce.More(head, env, out)
    case head :: tail =>
      val (v, o) = eval(head, env)
      if Value.isFalsy(v) then Bounce.Done(v, out + o)
      else evalAndBounce(tail, v, env, out + o)

  @tailrec
  private def evalOrBounce(
    args: List[Value],
    last: Value,
    env: Env,
    out: String
  ): Bounce = args match
    case Nil         => Bounce.Done(last, out)
    case head :: Nil => Bounce.More(head, env, out)
    case head :: tail =>
      val (v, o) = eval(head, env)
      if !Value.isFalsy(v) then Bounce.Done(v, out + o)
      else evalOrBounce(tail, v, env, out + o)
