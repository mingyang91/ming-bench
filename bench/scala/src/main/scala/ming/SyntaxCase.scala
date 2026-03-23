package ming

import SchemeValue.*

/** syntax-case macro system: pattern matching, syntax templates, and with-syntax. */
object SyntaxCase:

  /** Stack of (bindings, patternVars) for syntax-quote to access. Unlike syntax-rules, syntax-case templates do NOT
    * resolve free variables to definition-site values — they stay as symbols for use-site resolution. This provides
    * proper hygiene for introduced binding names.
    */
  private val emptyEnv: Environment                              = Environment()
  private var bindingsStack: List[(Macro.Bindings, Set[String])] = Nil

  def reset(): Unit =
    bindingsStack = Nil

  /** Evaluate (syntax-case expr (literals) clause ...). */
  def evalSyntaxCase(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): SchemeValue =
    args match
      case expr :: ListVal(litList, _) :: clauses =>
        val input    = Interpreter.eval(expr, env)
        val literals = litList.collect { case SymbolVal(n, _) => n }.toSet
        tryClauses(input, literals, clauses, env, pos)
      case _ => throw new EvalError("syntax-case: bad syntax")

  private def tryClauses(
    input: SchemeValue,
    literals: Set[String],
    clauses: List[SchemeValue],
    env: Environment,
    pos: Option[SourcePos]
  ): SchemeValue =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case ListVal(pattern :: template :: Nil, _) =>
          matchPattern(pattern, input, literals) match
            case Some((bindings, patVars)) =>
              return withBindings(bindings, patVars) {
                Interpreter.eval(template, env)
              }
            case None => remaining = remaining.tail
        case _ => throw new EvalError("syntax-case: bad clause")
    throw new EvalError("syntax-case: no matching pattern")

  /** Match a pattern against input using _ as wildcard. */
  private def matchPattern(
    pattern: SchemeValue,
    input: SchemeValue,
    literals: Set[String]
  ): Option[(Macro.Bindings, Set[String])] =
    Macro.matchForm(pattern, input, literals, "_").map { bindings =>
      val patVars = Macro.collectAllPatternVars(pattern, literals, "_")
      (bindings, patVars)
    }

  /** Evaluate (syntax-quote template) — the #'(...) form. */
  def evalSyntaxQuote(template: SchemeValue): SchemeValue =
    bindingsStack match
      case Nil =>
        throw new EvalError("syntax-quote: not inside syntax-case")
      case (bindings, patVars) :: _ =>
        Macro.expandTemplate(template, bindings, emptyEnv, patVars)

  /** Evaluate (with-syntax ((pattern expr) ...) body ...). */
  def evalWithSyntax(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): SchemeValue =
    args match
      case ListVal(bindingSpecs, _) :: body if body.nonEmpty =>
        val (currentBindings, currentPatVars) =
          bindingsStack.headOption.getOrElse((Map.empty, Set.empty[String]))

        var mergedBindings = currentBindings
        var mergedPatVars  = currentPatVars

        bindingSpecs.foreach {
          case ListVal(pattern :: expr :: Nil, _) =>
            val value = Interpreter.eval(expr, env)
            pattern match
              case SymbolVal(name, _) if name != "_" =>
                mergedBindings = mergedBindings + (name -> Left(value))
                mergedPatVars = mergedPatVars + name
              case _ =>
                Macro.matchForm(pattern, value, Set.empty, "_") match
                  case Some(b) =>
                    val patVars = Macro.collectAllPatternVars(pattern, Set.empty, "_")
                    mergedBindings = mergedBindings ++ b
                    mergedPatVars = mergedPatVars ++ patVars
                  case None =>
                    throw new EvalError("with-syntax: pattern did not match")
          case _ => throw new EvalError("with-syntax: bad binding")
        }

        withBindings(mergedBindings, mergedPatVars) {
          Interpreter.evalBodyInit(body, env)
          Interpreter.eval(body.last, env)
        }
      case _ => throw new EvalError("with-syntax: bad syntax")

  private def withBindings[T](
    bindings: Macro.Bindings,
    patVars: Set[String]
  )(f: => T): T =
    bindingsStack = (bindings, patVars) :: bindingsStack
    try f
    finally bindingsStack = bindingsStack.tail
