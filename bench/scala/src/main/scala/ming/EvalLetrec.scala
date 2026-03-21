package ming

object EvalLetrec:

  def evalLetrecBounce(args: List[Value], env: Env): Eval.Bounce = args match
    case Value.SList(bindings) :: body if body.nonEmpty =>
      val names = bindings.map {
        case Value.SList(Value.Symbol(n) :: _ :: Nil) => n
        case other                                    => throw new EvalError(s"bad letrec binding: ${other.display}")
      }
      val cells  = names.map(n => n -> Array[Value](Value.Void))
      val recEnv = Env(env.bindings ++ cells.toMap, env.parent, env.shared)
      val initOut = bindings.foldLeft("") { case (out, b) =>
        b match
          case Value.SList(Value.Symbol(n) :: expr :: Nil) =>
            val (v, o) = Eval.eval(expr, recEnv)
            recEnv.bindings(n)(0) = v
            out + o
          case _ => out
      }
      Eval.evalBodyBounce(body, recEnv, initOut)
    case _ => throw new EvalError("bad letrec syntax")

  def evalLetrecStarBounce(args: List[Value], env: Env): Eval.Bounce = args match
    case Value.SList(bindings) :: body if body.nonEmpty =>
      val names = bindings.map {
        case Value.SList(Value.Symbol(n) :: _ :: Nil) => n
        case other                                    => throw new EvalError(s"bad letrec* binding: ${other.display}")
      }
      val cells  = names.map(n => n -> Array[Value](Value.Void))
      val recEnv = Env(env.bindings ++ cells.toMap, env.parent, env.shared)
      val initOut = bindings.foldLeft("") { case (out, b) =>
        b match
          case Value.SList(Value.Symbol(n) :: expr :: Nil) =>
            val (v, o) = Eval.eval(expr, recEnv)
            recEnv.bindings(n)(0) = v
            out + o
          case _ => out
      }
      Eval.evalBodyBounce(body, recEnv, initOut)
    case _ => throw new EvalError("bad letrec* syntax")
