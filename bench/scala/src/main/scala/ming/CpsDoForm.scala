package ming

import SchemeValue.*
import InterpreterUtils.*
import Bounce.*

/** CPS handler for the `do` special form. */
object CpsDoForm:

  import CpsEval.{evalBodyK, evalK, evalSequenceK, Cont, Env}

  def evalDoK(
    rest: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    rest match
      case ListVal(bindings, _) :: ListVal(testAndResult, _) :: commands =>
        val (names, initExprs, stepExprs) = parseDoBindings(bindings)
        val (testExpr, resultExprs) = testAndResult match
          case test :: result => (test, result)
          case _              => throw new EvalError("do: bad test clause")
        evalDoInitsK(names, initExprs, stepExprs, testExpr, resultExprs, commands, env, out, k)
      case _ => throw new EvalError("do: bad syntax")

  private def parseDoBindings(
    bindings: List[SchemeValue]
  ): (List[String], List[SchemeValue], List[Option[SchemeValue]]) =
    val parsed = bindings.map {
      case ListVal(SymbolVal(name, _) :: init :: step :: Nil, _) =>
        (name, init, Some(step))
      case ListVal(SymbolVal(name, _) :: init :: Nil, _) =>
        (name, init, None)
      case _ => throw new EvalError("do: invalid binding")
    }
    (parsed.map(_._1), parsed.map(_._2), parsed.map(_._3))

  private def evalDoInitsK(
    names: List[String],
    initExprs: List[SchemeValue],
    stepExprs: List[Option[SchemeValue]],
    testExpr: SchemeValue,
    resultExprs: List[SchemeValue],
    commands: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    evalListK(
      initExprs,
      env,
      out,
      Nil,
      initVals =>
        val doEnv = names.zip(initVals).foldLeft(env) { case (e, (n, v)) =>
          e + (n -> makeCell(v))
        }
        doLoopK(names, stepExprs, testExpr, resultExprs, commands, doEnv, out, k)
    )

  private def evalListK(
    exprs: List[SchemeValue],
    env: Env,
    out: Array[String],
    acc: List[SchemeValue],
    k: List[SchemeValue] => Bounce
  ): Bounce =
    exprs match
      case Nil => More(() => k(acc.reverse))
      case head :: tail =>
        evalK(head, env, out, (v, _) => evalListK(tail, env, out, v :: acc, k))

  private def doLoopK(
    names: List[String],
    stepExprs: List[Option[SchemeValue]],
    testExpr: SchemeValue,
    resultExprs: List[SchemeValue],
    commands: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    evalK(
      testExpr,
      env,
      out,
      (testVal, _) =>
        if testVal.isTruthy then
          if resultExprs.isEmpty then More(() => k(Void, env))
          else evalBodyK(resultExprs, env, out, k)
        else
          evalSequenceK(
            commands,
            env,
            out,
            (_, cmdEnv) =>
              evalStepValsK(
                names,
                stepExprs,
                cmdEnv,
                out,
                Nil,
                stepVals =>
                  stepVals.foreach { case (name, v) =>
                    cmdEnv(name) match
                      case Cell(arr) => arr(0) = v
                      case _         => ()
                  }
                  doLoopK(names, stepExprs, testExpr, resultExprs, commands, cmdEnv, out, k)
              )
          )
    )

  private def evalStepValsK(
    names: List[String],
    stepExprs: List[Option[SchemeValue]],
    env: Env,
    out: Array[String],
    acc: List[(String, SchemeValue)],
    k: List[(String, SchemeValue)] => Bounce
  ): Bounce =
    (names, stepExprs) match
      case (Nil, Nil) => More(() => k(acc.reverse))
      case (name :: nTail, Some(stepExpr) :: sTail) =>
        evalK(
          stepExpr,
          env,
          out,
          (v, _) => evalStepValsK(nTail, sTail, env, out, (name, v) :: acc, k)
        )
      case (_ :: nTail, None :: sTail) =>
        evalStepValsK(nTail, sTail, env, out, acc, k)
      case _ => throw new EvalError("do: internal error")
