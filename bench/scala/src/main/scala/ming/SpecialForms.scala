package ming

import Evaluator.*
import Evaluator.Val.*

/** CPS special form handlers: define, if, cond, and, or, case, lambda, case-lambda. */
private[ming] object SpecialForms:

  def evalDefineK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(Pair(Symbol(name), params), body) =>
        val (paramNames, restParam) = parseParams(params)
        val bodyList                = Evaluator.toList(body)
        if bodyList.isEmpty then Evaluator.error("lambda: empty body")
        env.define(name, Closure(paramNames, restParam, bodyList, env))
        k(Void)
      case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
        Evaluator.evalK(
          valueExpr,
          env,
          v =>
            env.define(name, v)
            k(Void)
        )
      case _ => Evaluator.error("bad define syntax")

  def evalIfK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(cond, Pair(thenExpr, Pair(elseExpr, Nil))) =>
        Evaluator.evalK(
          cond,
          env,
          condVal =>
            if condVal != Bool(false) then Evaluator.evalK(thenExpr, env, k)
            else Evaluator.evalK(elseExpr, env, k)
        )
      case Pair(cond, Pair(thenExpr, Nil)) =>
        Evaluator.evalK(
          cond,
          env,
          condVal =>
            if condVal != Bool(false) then Evaluator.evalK(thenExpr, env, k)
            else k(Void)
        )
      case _ => Evaluator.error("bad if syntax")

  def evalCondK(clauses: Val, env: Env, k: Cont): Bounce =
    clauses match
      case Nil => k(Void)
      case Pair(clause, rest) =>
        val clauseList = Evaluator.toList(clause)
        if clauseList.isEmpty then Evaluator.error("bad cond clause")
        clauseList.head match
          case Symbol("else") => Evaluator.evalSeqK(clauseList.tail, env, k)
          case test =>
            Evaluator.evalK(
              test,
              env,
              v =>
                if v != Bool(false) then
                  if clauseList.tail.isEmpty then k(v)
                  else Evaluator.evalSeqK(clauseList.tail, env, k)
                else BMore(() => evalCondK(rest, env, k))
            )
      case _ => Evaluator.error("bad cond syntax")

  def evalAndK(args: List[Val], env: Env, k: Cont): Bounce =
    args match
      case scala.Nil         => k(Bool(true))
      case last :: scala.Nil => Evaluator.evalK(last, env, k)
      case head :: tail =>
        Evaluator.evalK(
          head,
          env,
          v =>
            if v == Bool(false) then k(Bool(false))
            else BMore(() => evalAndK(tail, env, k))
        )

  def evalOrK(args: List[Val], env: Env, k: Cont): Bounce =
    args match
      case scala.Nil         => k(Bool(false))
      case last :: scala.Nil => Evaluator.evalK(last, env, k)
      case head :: tail =>
        Evaluator.evalK(
          head,
          env,
          v =>
            if v != Bool(false) then k(v)
            else BMore(() => evalOrK(tail, env, k))
        )

  def evalCaseK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(keyExpr, clauses) =>
        Evaluator.evalK(
          keyExpr,
          env,
          key =>
            def matchClauses(cls: Val): Bounce =
              cls match
                case Nil => k(Void)
                case Pair(clause, remaining) =>
                  val clauseList = Evaluator.toList(clause)
                  if clauseList.isEmpty then Evaluator.error("bad case clause")
                  clauseList.head match
                    case Symbol("else") => Evaluator.evalSeqK(clauseList.tail, env, k)
                    case datums =>
                      val datumList = Evaluator.toList(datums)
                      if datumList.exists(d => d == key) then Evaluator.evalSeqK(clauseList.tail, env, k)
                      else BMore(() => matchClauses(remaining))
                case _ => Evaluator.error("bad case syntax")
            matchClauses(clauses)
        )
      case _ => Evaluator.error("bad case syntax")

  def evalCaseLambda(clausesList: Val, env: Env): Val =
    val clauses = Evaluator.toList(clausesList).map { clause =>
      clause match
        case Pair(params, body) =>
          val (paramNames, restParam) = parseParams(params)
          val bodyList                = Evaluator.toList(body)
          if bodyList.isEmpty then Evaluator.error("case-lambda: empty body")
          (paramNames, restParam, bodyList)
        case _ => Evaluator.error("bad case-lambda clause")
    }
    Builtin { args =>
      clauses.find { (paramNames, restParam, _) =>
        if restParam.isDefined then args.length >= paramNames.length
        else args.length == paramNames.length
      } match
        case Some((paramNames, restParam, bodyList)) =>
          val childEnv = Env.empty(Some(env))
          paramNames.zip(args).foreach((p, a) => childEnv.define(p, a))
          restParam.foreach { rp =>
            val restArgs = args.drop(paramNames.length)
            childEnv.define(rp, restArgs.foldRight(Nil: Val)((a, acc) => Pair(a, acc)))
          }
          var result: Val = Void
          for expr <- bodyList do result = Interpreter.eval(expr, childEnv)
          result
        case None =>
          Evaluator.error(s"case-lambda: no matching clause for ${args.length} arguments")
    }

  def evalLambda(rest: Val, env: Env): Val =
    rest match
      case Pair(params, body) =>
        val (paramNames, restParam) = parseParams(params)
        val bodyList                = Evaluator.toList(body)
        if bodyList.isEmpty then Evaluator.error("lambda: empty body")
        Closure(paramNames, restParam, bodyList, env)
      case _ => Evaluator.error("bad lambda syntax")

  def parseParams(params: Val): (List[String], Option[String]) =
    params match
      case Nil          => (List.empty, None)
      case Symbol(name) => (List.empty, Some(name))
      case Pair(Symbol(name), rest) =>
        rest match
          case Symbol(restName) => (List(name), Some(restName))
          case _ =>
            val (more, restParam) = parseParams(rest)
            (name :: more, restParam)
      case _ => Evaluator.error(s"bad parameter: ${Display.write(params)}")
