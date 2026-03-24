package ming

import Evaluator.Val
import Evaluator.Val.*

/** Syntax-case macro expansion engine. */
private[ming] object SyntaxCase:

  private def error(msg: String): Nothing =
    throw new EvalError(msg)

  // --- syntax-case bindings state (stack for nesting) ---
  private[ming] var syntaxCaseBindings: Map[String, Either[Val, List[Val]]] = Map.empty
  private[ming] var syntaxCasePatVars: Set[String]                          = Set.empty
  private[ming] var syntaxCaseDefEnv: Option[Env]                           = None
  private[ming] var syntaxCaseDefNames: Option[Set[String]]                 = None

  /** Create a syntax-case transformer from a lambda closure. */
  def makeTransformer(closure: Val, defEnv: Env, defNames: Set[String]): Val => Val =
    (form: Val) =>
      val savedBindings = syntaxCaseBindings
      val savedPatVars  = syntaxCasePatVars
      val savedDefEnv   = syntaxCaseDefEnv
      val savedDefNames = syntaxCaseDefNames
      syntaxCaseBindings = Map.empty
      syntaxCasePatVars = Set.empty
      syntaxCaseDefEnv = Some(defEnv)
      syntaxCaseDefNames = Some(defNames)
      try Interpreter.applyFunc(closure, List(form))
      finally
        syntaxCaseBindings = savedBindings
        syntaxCasePatVars = savedPatVars
        syntaxCaseDefEnv = savedDefEnv
        syntaxCaseDefNames = savedDefNames

  /** Evaluate (syntax-case expr (literals...) clause ...) */
  def evalSyntaxCaseK(rest: Val, env: Env, k: Evaluator.Cont): Evaluator.Bounce =
    val elems = Evaluator.toList(rest)
    if elems.length < 2 then error("syntax-case: bad form")
    val stxExpr      = elems.head
    val literalsList = elems(1)
    val clauses      = elems.drop(2)
    val literals = Evaluator.toList(literalsList).map {
      case Symbol(s) => s
      case _         => error("syntax-case: literals must be identifiers")
    }
    Evaluator.evalK(stxExpr, env, stxVal => trySyntaxCaseClauses(stxVal, literals, clauses, env, k))

  private def trySyntaxCaseClauses(
    input: Val,
    literals: List[String],
    clauses: List[Val],
    env: Env,
    k: Evaluator.Cont
  ): Evaluator.Bounce =
    clauses match
      case scala.Nil => error(s"syntax-case: no matching pattern for ${Display.write(input)}")
      case clause :: rest =>
        val clauseElems = Evaluator.toList(clause)
        val (pattern, fender, body) = clauseElems match
          case List(pat, bod)       => (pat, None, bod)
          case List(pat, fend, bod) => (pat, Some(fend), bod)
          case _                    => error("syntax-case: bad clause")
        matchSyntaxCasePattern(pattern, input, literals) match
          case None => trySyntaxCaseClauses(input, literals, rest.toList, env, k)
          case Some((bindings, patVars)) =>
            val savedBindings = syntaxCaseBindings
            val savedPatVars  = syntaxCasePatVars
            syntaxCaseBindings = savedBindings ++ bindings
            syntaxCasePatVars = savedPatVars ++ patVars
            val clauseEnv = Env.empty(Some(env))
            bindings.foreach { (name, v) =>
              v match
                case Left(value) => clauseEnv.define(name, value)
                case Right(values) =>
                  clauseEnv.define(name, values.foldRight(Nil: Val)((a, acc) => Pair(a, acc)))
            }
            def evalBody(): Evaluator.Bounce =
              Evaluator.evalK(
                body,
                clauseEnv,
                result =>
                  syntaxCaseBindings = savedBindings
                  syntaxCasePatVars = savedPatVars
                  k(result)
              )
            fender match
              case None => evalBody()
              case Some(fend) =>
                Evaluator.evalK(
                  fend,
                  clauseEnv,
                  fenderResult =>
                    fenderResult match
                      case Bool(false) =>
                        syntaxCaseBindings = savedBindings
                        syntaxCasePatVars = savedPatVars
                        trySyntaxCaseClauses(input, literals, rest.toList, env, k)
                      case _ => evalBody()
                )

  /** Match a syntax-case pattern. */
  private def matchSyntaxCasePattern(
    pattern: Val,
    input: Val,
    literals: List[String]
  ): Option[(Map[String, Either[Val, List[Val]]], Set[String])] =
    (pattern, input) match
      case (Pair(_, patRest), Pair(_, inpRest)) =>
        val patElems = Macros.toElements(patRest)
        val inpElems = Evaluator.toList(inpRest)
        Macros.matchPatternList(patElems, inpElems, literals).map { bindings =>
          val kwPat = pattern match
            case Pair(kw, _) => kw
            case _           => Nil
          val kwInp = input match
            case Pair(kw, _) => kw
            case _           => Nil
          val kwBindings = kwPat match
            case Symbol(name) if name != "_" && !literals.contains(name) =>
              Map(name -> Left(kwInp))
            case _ => Map.empty[String, Either[Val, List[Val]]]
          val allBindings = kwBindings ++ bindings
          val patVars     = Macros.collectPatternVars(pattern, literals)
          (allBindings, patVars)
        }
      case (Symbol(name), _) if name != "_" && !literals.contains(name) =>
        Some((Map(name -> Left(input)), Set(name)))
      case _ => None

  /** Expand a (syntax template) form using current syntax-case bindings. */
  def expandSyntaxTemplate(template: Val, env: Env): Val =
    expandSyntaxCaseInner(template, syntaxCaseBindings, syntaxCasePatVars)

  private def expandSyntaxCaseInner(
    template: Val,
    bindings: Map[String, Either[Val, List[Val]]],
    patVars: Set[String]
  ): Val =
    template match
      case Symbol(name) if bindings.contains(name) =>
        bindings(name) match
          case Left(v)  => v
          case Right(_) => error(s"ellipsis variable $name used without ...")
      case Symbol(_)                => template
      case Pair(Symbol("quote"), _) => template
      case Pair(_, _) =>
        val elems    = Macros.toElements(template)
        val expanded = expandSyntaxCaseElements(elems, bindings, patVars)
        expanded.foldRight(Nil: Val)((a, acc) => Pair(a, acc))
      case _ => template

  private def expandSyntaxCaseElements(
    elems: List[Val],
    bindings: Map[String, Either[Val, List[Val]]],
    patVars: Set[String]
  ): List[Val] =
    elems match
      case scala.Nil => scala.Nil
      case tmpl :: Symbol("...") :: rest =>
        val ellipsisVars = Macros.findEllipsisVars(tmpl, bindings)
        if ellipsisVars.isEmpty then expandSyntaxCaseElements(rest, bindings, patVars)
        else
          val count = bindings(ellipsisVars.head) match
            case Right(vs) => vs.length
            case _         => 0
          val expanded = (0 until count).toList.map { i =>
            val singleBindings = bindings.map { (k, v) =>
              if ellipsisVars.contains(k) then
                v match
                  case Right(vs) => (k, Left(vs(i)))
                  case other     => (k, other)
              else (k, v)
            }
            expandSyntaxCaseInner(tmpl, singleBindings, patVars)
          }
          expanded ++ expandSyntaxCaseElements(rest, bindings, patVars)
      case tmpl :: rest =>
        expandSyntaxCaseInner(tmpl, bindings, patVars) ::
          expandSyntaxCaseElements(rest, bindings, patVars)

  /** Evaluate (with-syntax ((pattern expr) ...) body ...) */
  def evalWithSyntaxK(rest: Val, env: Env, k: Evaluator.Cont): Evaluator.Bounce =
    val elems = Evaluator.toList(rest)
    if elems.length < 2 then error("with-syntax: bad form")
    val bindingsList = Evaluator.toList(elems.head)
    val body         = elems.tail
    evalWithSyntaxBindings(bindingsList, env, k, body)

  private def evalWithSyntaxBindings(
    bindings: List[Val],
    env: Env,
    k: Evaluator.Cont,
    body: List[Val]
  ): Evaluator.Bounce =
    bindings match
      case scala.Nil =>
        Evaluator.evalSeqK(body, env, k)
      case binding :: rest =>
        val bindElems = Evaluator.toList(binding)
        if bindElems.length != 2 then error("with-syntax: bad binding")
        val pattern = bindElems.head
        val expr    = bindElems(1)
        Evaluator.evalK(
          expr,
          env,
          value =>
            val patVars = Macros.collectPatternVars(pattern, List.empty)
            val matched = Macros.matchPatternSingle(value, List.empty, pattern)
            matched match
              case Some(newBindings) =>
                syntaxCaseBindings = syntaxCaseBindings ++ newBindings
                syntaxCasePatVars = syntaxCasePatVars ++ patVars
                newBindings.foreach { (name, v) =>
                  v match
                    case Left(value) => env.define(name, value)
                    case Right(_)    => ()
                }
                evalWithSyntaxBindings(rest, env, k, body)
              case None => error("with-syntax: pattern match failed")
        )
