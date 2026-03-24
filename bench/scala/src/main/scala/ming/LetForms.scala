package ming

import Evaluator.Val
import Evaluator.Val.*

/** Evaluators for let, cond, and case-lambda special forms. */
private[ming] object LetForms:

  def evalLet(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    rest match
      // Named let: (let name ((var init) ...) body ...)
      case Pair(Symbol(name), Pair(bindings, body)) =>
        val bindingList = Evaluator.toList(bindings)
        val paramNames = bindingList.map {
          case Pair(Symbol(p), Pair(_, Nil)) => p
          case _                             => error("bad named let binding")
        }
        val initVals = bindingList.map {
          case Pair(_, Pair(v, Nil)) => eval(v, env)
          case _                     => error("bad named let binding")
        }
        val bodyList = Evaluator.toList(body)
        if bodyList.isEmpty then error("let: empty body")
        val childEnv = Env.empty(Some(env))
        val loopFunc = Builtin { args =>
          if args.length != paramNames.length then
            error(s"named let $name: expected ${paramNames.length} arguments, got ${args.length}")
          val loopEnv = Env.empty(Some(childEnv))
          paramNames.zip(args).foreach((p, a) => loopEnv.define(p, a))
          var result: Val = Void
          for expr <- bodyList do result = eval(expr, loopEnv)
          result
        }
        childEnv.define(name, loopFunc)
        val loopEnv = Env.empty(Some(childEnv))
        paramNames.zip(initVals).foreach((p, a) => loopEnv.define(p, a))
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, loopEnv)
        result
      // Regular let: (let ((var init) ...) body ...)
      case Pair(bindings, body) =>
        val childEnv = Env.empty(Some(env))
        for binding <- Evaluator.toList(bindings) do
          binding match
            case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
              childEnv.define(name, eval(valueExpr, env))
            case _ => error("bad let binding")
        val bodyList = Evaluator.toList(body)
        if bodyList.isEmpty then error("let: empty body")
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, childEnv)
        result
      case _ => error("bad let syntax")

  def evalCond(
    clauses: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): Val =
    clauses match
      case Nil => Void
      case Pair(clause, rest) =>
        val clauseList = Evaluator.toList(clause)
        if clauseList.isEmpty then error("bad cond clause")
        clauseList.head match
          case Symbol("else") =>
            var result: Val = Void
            for expr <- clauseList.tail do result = eval(expr, env)
            result
          case test =>
            val v = eval(test, env)
            if v != Bool(false) then
              if clauseList.tail.isEmpty then v
              else
                var result: Val = Void
                for expr <- clauseList.tail do result = eval(expr, env)
                result
            else evalCond(rest, env, eval, error)
      case _ => error("bad cond syntax")
