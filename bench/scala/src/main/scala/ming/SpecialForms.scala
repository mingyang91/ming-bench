package ming

import Evaluator.Val
import Evaluator.Val.*

/** Evaluators for case-lambda, let*, letrec, letrec*, case, and do special forms. */
private[ming] object SpecialForms:

  def evalCaseLambda(
    clausesList: Val,
    env: Env,
    eval: (Val, Env) => Val,
    parseParams: Val => (List[String], Option[String]),
    error: String => Nothing
  ): Val =
    val clauses = Evaluator.toList(clausesList).map { clause =>
      clause match
        case Pair(params, body) =>
          val (paramNames, restParam) = parseParams(params)
          val bodyList                = Evaluator.toList(body)
          if bodyList.isEmpty then error("case-lambda: empty body")
          (paramNames, restParam, bodyList)
        case _ => error("bad case-lambda clause")
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
          for expr <- bodyList do result = eval(expr, childEnv)
          result
        case None =>
          error(s"case-lambda: no matching clause for ${args.length} arguments")
    }

  def evalLetStar(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    rest match
      case Pair(bindings, body) =>
        val childEnv = Env.empty(Some(env))
        for binding <- Evaluator.toList(bindings) do
          binding match
            case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
              childEnv.define(name, eval(valueExpr, childEnv))
            case _ => error("bad let* binding")
        val bodyList = Evaluator.toList(body)
        if bodyList.isEmpty then error("let*: empty body")
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, childEnv)
        result
      case _ => error("bad let* syntax")

  def evalLetrec(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    rest match
      case Pair(bindings, body) =>
        val childEnv    = Env.empty(Some(env))
        val bindingList = Evaluator.toList(bindings)
        val names = bindingList.map {
          case Pair(Symbol(name), Pair(_, Nil)) => name
          case _                                => error("bad letrec binding")
        }
        names.foreach(n => childEnv.define(n, Void))
        bindingList.foreach {
          case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
            childEnv.define(name, eval(valueExpr, childEnv))
          case _ => error("bad letrec binding")
        }
        val bodyList = Evaluator.toList(body)
        if bodyList.isEmpty then error("letrec: empty body")
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, childEnv)
        result
      case _ => error("bad letrec syntax")

  def evalLetrecStar(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    rest match
      case Pair(bindings, body) =>
        val childEnv = Env.empty(Some(env))
        for binding <- Evaluator.toList(bindings) do
          binding match
            case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
              childEnv.define(name, eval(valueExpr, childEnv))
            case _ => error("bad letrec* binding")
        val bodyList = Evaluator.toList(body)
        if bodyList.isEmpty then error("letrec*: empty body")
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, childEnv)
        result
      case _ => error("bad letrec* syntax")

  def evalCase(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    rest match
      case Pair(keyExpr, clauses) =>
        val key = eval(keyExpr, env)
        def matchClauses(cls: Val): Val = cls match
          case Nil => Void
          case Pair(clause, remaining) =>
            val clauseList = Evaluator.toList(clause)
            if clauseList.isEmpty then error("bad case clause")
            clauseList.head match
              case Symbol("else") =>
                var result: Val = Void
                for expr <- clauseList.tail do result = eval(expr, env)
                result
              case datums =>
                val datumList = Evaluator.toList(datums)
                if datumList.exists(d => d == key) then
                  var result: Val = Void
                  for expr <- clauseList.tail do result = eval(expr, env)
                  result
                else matchClauses(remaining)
          case _ => error("bad case syntax")
        matchClauses(clauses)
      case _ => error("bad case syntax")

  def evalDo(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    rest match
      case Pair(varSpecs, Pair(testClause, body)) =>
        val specs = Evaluator.toList(varSpecs).map { spec =>
          Evaluator.toList(spec) match
            case List(Symbol(name), init)       => (name, init, None: Option[Val])
            case List(Symbol(name), init, step) => (name, init, Some(step))
            case _                              => error("bad do variable spec")
        }
        val testList = Evaluator.toList(testClause)
        if testList.isEmpty then error("bad do test clause")
        val testExpr    = testList.head
        val resultExprs = testList.tail
        val bodyExprs   = Evaluator.toList(body)
        val doEnv       = Env.empty(Some(env))
        for (name, init, _) <- specs do doEnv.define(name, eval(init, env))
        while true do
          val testVal = eval(testExpr, doEnv)
          if testVal != Bool(false) then
            var result: Val = Void
            for expr <- resultExprs do result = eval(expr, doEnv)
            return result
          for expr <- bodyExprs do eval(expr, doEnv)
          val newVals = specs.map { (name, _, step) =>
            step match
              case Some(stepExpr) => eval(stepExpr, doEnv)
              case None           => doEnv.lookup(name).getOrElse(throw new EvalError(s"unbound: $name"))
          }
          specs.zip(newVals).foreach { case ((name, _, _), v) => doEnv.define(name, v) }
        Void
      case _ => error("bad do syntax")
