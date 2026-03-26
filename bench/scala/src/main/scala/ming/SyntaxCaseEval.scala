package ming

import scala.collection.mutable

private[ming] object SyntaxCaseEval:

  def evalSyntaxCaseForm(args: List[Expr], env: Env, k: Kont): MState =
    if args.size < 2 then throw new EvalError("syntax-case: bad syntax")
    val stxExpr            = args.head
    val SList(litExprs, _) = args(1): @unchecked
    val literals = litExprs.map {
      case Symbol(n, _) => n; case _ => throw new EvalError("syntax-case: bad literal")
    }.toSet
    val clauses = args.drop(2)
    SEval(stxExpr, env, SyntaxCaseK(literals, clauses, env, k))

  def applySyntaxCase(
    stxVal: SchemeVal,
    literals: Set[String],
    clauses: List[Expr],
    env: Env,
    k: Kont
  ): MState =
    val inputExpr = stxVal match
      case SchemeSyntax(e) => e
      case _               => throw new EvalError("syntax-case: expected syntax object")

    val macroName = inputExpr match
      case SList(Symbol(n, _) :: _, _) => n
      case _                           => ""

    clauses.iterator
      .map {
        case SList(elems, _) if elems.size >= 2 => elems
        case _                                  => throw new EvalError("syntax-case: bad clause")
      }
      .flatMap { clauseElems =>
        Macros.matchPattern(inputExpr, clauseElems.head, literals, macroName).map { bindings =>
          val clauseEnv = new Env(mutable.Map.empty, Some(env))
          for (name, binding) <- bindings do
            binding match
              case Macros.SingleBinding(expr) => clauseEnv.set(name, SchemeSyntax(expr))
              case Macros.ListBinding(exprs)  => clauseEnv.set(name, SchemeSyntaxList(exprs))
          SEval(clauseElems.last, clauseEnv, k)
        }
      }
      .nextOption()
      .getOrElse(throw new EvalError(s"syntax-case: no matching pattern"))

  def evalWithSyntaxForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SList(Symbol(name, _) :: initExpr :: Nil, _) =>
              val value = Evaluator.eval(initExpr, env)
              localEnv.set(name, value)
            case _ => throw new EvalError("with-syntax: bad binding")
        Evaluator.evalBodyCEK(body, localEnv, k)
      case _ => throw new EvalError("with-syntax: bad syntax")
