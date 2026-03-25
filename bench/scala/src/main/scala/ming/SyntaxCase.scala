package ming

import Evaluator.{Bounce, Cont, Done, More}

object SyntaxCase:
  // Current pattern bindings from enclosing syntax-case / with-syntax forms
  var patternBindings: Macro.Bindings = Map.empty

  /** Evaluate (syntax-case expr (literals) clause ...) */
  def evalSyntaxCaseK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size < 3 then throw new EvalError("syntax-case: bad syntax")
    val exprForm = args.head
    val literals = args(1) match
      case SchemeVal.SList(lits) =>
        lits.map {
          case SchemeVal.Symbol(s) => s
          case other               => throw new EvalError(s"syntax-case: expected literal, got $other")
        }
      case _ => throw new EvalError("syntax-case: expected literal list")
    val clauses = args.drop(2)

    Evaluator.evalK(exprForm, env, inputVal => tryClausesK(inputVal, literals, clauses, env, k))

  private def tryClausesK(
    input: SchemeVal,
    literals: List[String],
    clauses: List[SchemeVal],
    env: Env,
    k: Cont
  ): Bounce =
    clauses match
      case Nil => throw new EvalError("syntax-case: no matching clause")
      case clause :: rest =>
        clause match
          case SchemeVal.SList(elems) if elems.size >= 2 =>
            val pattern = elems.head
            val (fender, body) =
              if elems.size == 3 then (Some(elems(1)), elems(2))
              else if elems.size == 2 then (None, elems(1))
              else throw new EvalError("syntax-case: bad clause")

            Macro.matchPattern(pattern, input, literals) match
              case Some(bindings) =>
                val saved  = patternBindings
                val merged = bindings ++ patternBindings
                fender match
                  case Some(fenderExpr) =>
                    patternBindings = merged
                    Evaluator.evalK(
                      fenderExpr,
                      env,
                      fenderVal =>
                        if Evaluator.isTruthy(fenderVal) then
                          Evaluator.evalK(
                            body,
                            env,
                            result =>
                              patternBindings = saved
                              k(result)
                          )
                        else
                          patternBindings = saved
                          tryClausesK(input, literals, rest, env, k)
                    )
                  case None =>
                    patternBindings = merged
                    Evaluator.evalK(
                      body,
                      env,
                      result =>
                        patternBindings = saved
                        k(result)
                    )
              case None =>
                tryClausesK(input, literals, rest, env, k)
          case _ => throw new EvalError("syntax-case: bad clause")

  /** Evaluate (syntax template) — expand template using current pattern bindings. */
  def evalSyntax(template: SchemeVal): SchemeVal =
    Macro.instantiateTemplate(template, patternBindings)

  /** Evaluate (with-syntax ((pattern expr) ...) body ...) */
  def evalWithSyntaxK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size < 2 then throw new EvalError("with-syntax: bad syntax")
    val bindingForms = args.head match
      case SchemeVal.SList(bs) => bs
      case _                   => throw new EvalError("with-syntax: expected binding list")
    val body = args.tail

    evalBindingsK(
      bindingForms,
      Map.empty,
      env,
      newBindings =>
        val saved = patternBindings
        patternBindings = newBindings ++ patternBindings
        Evaluator.evalBodyK(
          body,
          env,
          result =>
            patternBindings = saved
            k(result)
        )
    )

  private def evalBindingsK(
    forms: List[SchemeVal],
    acc: Macro.Bindings,
    env: Env,
    k: Macro.Bindings => Bounce
  ): Bounce =
    forms match
      case Nil => k(acc)
      case SchemeVal.SList(List(pattern, expr)) :: rest =>
        Evaluator.evalK(
          expr,
          env,
          value =>
            Macro.matchPattern(pattern, value, Nil) match
              case Some(bindings) => evalBindingsK(rest, acc ++ bindings, env, k)
              case None           => throw new EvalError("with-syntax: pattern match failed")
        )
      case _ => throw new EvalError("with-syntax: bad binding")
