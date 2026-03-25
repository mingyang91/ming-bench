package ming

/** Procedure application logic. */
object Apply:

  def apply(proc: SchemeVal, args: List[SchemeVal]): SchemeVal = proc match
    case SchemeVal.BuiltinProc(_, f) => f(args)
    case SchemeVal.LambdaProc(params, body, closure, rest) =>
      rest match
        case Some(restName) =>
          if args.size < params.size then
            throw new EvalError(s"expected at least ${params.size} arguments, got ${args.size}")
          val localEnv = new Env(scala.collection.mutable.Map.empty, Some(closure))
          params.zip(args).foreach((p, a) => localEnv.define(p, a))
          localEnv.define(restName, SchemeVal.SList(args.drop(params.size)))
          body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
        case None =>
          if args.size != params.size then throw new EvalError(s"expected ${params.size} arguments, got ${args.size}")
          val localEnv = new Env(scala.collection.mutable.Map.empty, Some(closure))
          params.zip(args).foreach((p, a) => localEnv.define(p, a))
          body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
    case SchemeVal.CaseLambdaProc(clauses, closure) =>
      val matched = clauses.find { case (params, rest, _) =>
        rest match
          case Some(_) => args.size >= params.size
          case None    => args.size == params.size
      }
      matched match
        case Some((params, rest, body)) =>
          val localEnv = new Env(scala.collection.mutable.Map.empty, Some(closure))
          params.zip(args).foreach((p, a) => localEnv.define(p, a))
          rest.foreach(r => localEnv.define(r, SchemeVal.SList(args.drop(params.size))))
          body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
        case None =>
          throw new EvalError(s"case-lambda: no matching clause for ${args.size} arguments")
    case _ => throw new EvalError(s"not a procedure: $proc")
