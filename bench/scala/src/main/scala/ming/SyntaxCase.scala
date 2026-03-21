package ming

import Evaluator.{Bounce, Done, EvalResult}
import Macros.{Binding, Ellipsis, Single}

/** syntax-case macro system: pattern matching, template construction, hygiene. */
object SyntaxCase:

  /** Evaluate (syntax-case expr (literals) clause ...) */
  def evalSyntaxCase(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case expr :: litsVal :: clauses if clauses.nonEmpty =>
        val (stxVal, _, out2) = Evaluator.eval(expr, env, out)
        val lits = Evaluator.toList(litsVal).map {
          case Value.Symbol(n, _) => n
          case _ =>
            throw new EvalError("syntax-case: literals must be identifiers")
        }
        val inputList = Evaluator.toList(stxVal)
        matchClauses(inputList, lits, clauses, env, out2)
      case _ => throw new EvalError("bad syntax-case syntax")

  private def matchClauses(
    inputList: List[Value],
    lits: List[String],
    clauses: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    clauses match
      case Nil =>
        throw new EvalError("no matching syntax-case pattern")
      case clause :: rest =>
        val elems = Evaluator.toList(clause)
        dispatchClause(elems, inputList, lits, rest, env, out)

  private def dispatchClause(
    elems: List[Value],
    inputList: List[Value],
    lits: List[String],
    rest: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    elems match
      case pat :: body :: Nil =>
        tryMatch(pat, inputList, lits) match
          case Some(binds) =>
            evalWithBindings(body, binds, env, out)
          case None =>
            matchClauses(inputList, lits, rest, env, out)
      case pat :: fender :: body :: Nil =>
        tryMatchWithFender(
          pat,
          fender,
          body,
          inputList,
          lits,
          rest,
          env,
          out
        )
      case _ => throw new EvalError("bad syntax-case clause")

  private def tryMatchWithFender(
    pat: Value,
    fender: Value,
    body: Value,
    inputList: List[Value],
    lits: List[String],
    rest: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    tryMatch(pat, inputList, lits) match
      case Some(binds) =>
        val fEnv               = bindingsEnv(binds, env)
        val (fResult, _, out2) = Evaluator.eval(fender, fEnv, out)
        if !Evaluator.isFalsy(fResult) then evalWithBindings(body, binds, env, out2)
        else matchClauses(inputList, lits, rest, env, out2)
      case None =>
        matchClauses(inputList, lits, rest, env, out)

  private def tryMatch(
    pat: Value,
    inputList: List[Value],
    lits: List[String]
  ): Option[Map[String, Binding]] =
    Macros.matchElems(
      Evaluator.toList(pat).tail,
      inputList.tail,
      lits,
      Map.empty
    )

  private def bindingsEnv(
    binds: Map[String, Binding],
    env: Env
  ): Env =
    env.define(
      "__syntax_bindings__",
      Value.SyntaxBindingsVal(binds, env)
    )

  private def evalWithBindings(
    body: Value,
    binds: Map[String, Binding],
    env: Env,
    out: String
  ): EvalResult =
    Bounce(body, bindingsEnv(binds, env), out)

  /** Evaluate (syntax template) — substitute pattern vars, apply hygiene. */
  def evalSyntax(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case tmpl :: Nil =>
        val bindsVal =
          try env.lookup("__syntax_bindings__")
          catch case _: EvalError => Value.NilVal
        bindsVal match
          case Value.SyntaxBindingsVal(binds, _) =>
            val expanded = instantiateTemplate(tmpl, binds)
            Done(expanded, env, out)
          case _ =>
            Done(tmpl, env, out)
      case _ => throw new EvalError("syntax requires exactly 1 argument")

  /** Evaluate (with-syntax ((pat expr) ...) body ...) */
  def evalWithSyntax(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case bindings :: body if body.nonEmpty =>
        val existing    = existingBindings(env)
        val bindingList = Evaluator.toList(bindings)
        val (merged, out2) =
          bindingList.foldLeft((existing, out)) { case ((acc, o), binding) =>
            processWithSyntaxBinding(binding, acc, env, o)
          }
        val newEnv = env.define(
          "__syntax_bindings__",
          Value.SyntaxBindingsVal(merged, env)
        )
        Evaluator.evalBodyTail(body, newEnv, out2)
      case _ => throw new EvalError("bad with-syntax syntax")

  private def processWithSyntaxBinding(
    binding: Value,
    acc: Map[String, Binding],
    env: Env,
    out: String
  ): (Map[String, Binding], String) =
    Evaluator.toList(binding) match
      case pat :: expr :: Nil =>
        val (v, _, o2) = Evaluator.eval(expr, env, out)
        pat match
          case Value.Symbol(name, _) =>
            (acc + (name -> Single(v)), o2)
          case _ =>
            throw new EvalError("bad with-syntax pattern")
      case _ => throw new EvalError("bad with-syntax binding")

  private def existingBindings(
    env: Env
  ): Map[String, Binding] =
    try
      env.lookup("__syntax_bindings__") match
        case Value.SyntaxBindingsVal(b, _) => b
        case _                             => Map.empty
    catch case _: EvalError => Map.empty

  /** Substitute pattern variables into template without hygiene renaming. syntax-case templates resolve free references
    * at the use site.
    */
  private def instantiateTemplate(
    tmpl: Value,
    binds: Map[String, Binding]
  ): Value =
    Macros.subst(tmpl, binds, Map.empty)

  /** Handle syntax->datum: unwrap syntax to datum (identity in our model). */
  def syntaxToDatum(args: List[Value]): Value =
    args match
      case v :: Nil => v
      case _ =>
        throw new EvalError("syntax->datum requires 1 argument")

  /** Handle datum->syntax: wrap datum as syntax (identity in our model). */
  def datumToSyntax(args: List[Value]): Value =
    args match
      case _ :: datum :: Nil => datum
      case _ =>
        throw new EvalError("datum->syntax requires 2 arguments")
