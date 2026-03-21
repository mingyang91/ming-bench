package ming

import scala.annotation.tailrec

/** Hygienic macro expansion for define-syntax / syntax-rules. */
object Macro:

  private case class MatchResult(
    bindings: Map[String, Value],
    ellipsis: Map[String, List[Value]]
  )

  /** Parse a define-syntax form and return (Void, newEnv, output). */
  def evalDefineSyntax(args: List[Value], env: Env): (Value, Env, String) =
    args match
      case Value.Symbol(name) :: Value.SList(Value.Symbol("syntax-rules") :: rest) :: Nil =>
        val (literals, rules) = parseSyntaxRules(rest)
        val mac               = Value.Macro(literals, rules, env)
        Eval.updateSharedCell(env, name, mac)
        (Value.Void, env.extend(name, mac), "")
      case Value.Symbol(name) :: transformerExpr :: Nil =>
        val (transformer, out) = Eval.eval(transformerExpr, env)
        val mac                = Value.TransformerMacro(transformer, env)
        Eval.updateSharedCell(env, name, mac)
        (Value.Void, env.extend(name, mac), out)
      case _ => throw new EvalError("bad define-syntax syntax")

  /** Check if name resolves to a macro; if so expand and return (expanded, env). */
  def tryApply(name: String, form: List[Value], env: Env): Option[(Value, Env)] =
    val resolved =
      try Some(env.lookup(name))
      catch case _: EvalError => None
    resolved match
      case Some(mac: Value.Macro) =>
        val (expanded, freeBindings) = expand(mac, form)
        val localEnv                 = freeBindings.foldLeft(env) { case (e, (n, v)) => e.extend(n, v) }
        Some((expanded, localEnv))
      case Some(mac: Value.TransformerMacro) =>
        val (expanded, localEnv) = SyntaxCase.applyTransformer(mac, form, env)
        Some((expanded, localEnv))
      case _ => None

  private def expand(mac: Value.Macro, form: List[Value]): (Value, Map[String, Value]) =
    val Value.Macro(literals, rules, defEnv) = mac
    val matched = rules.iterator
      .map { case (pattern, template) => matchRule(pattern, form, literals).map(r => (r, template)) }
      .collectFirst { case Some(result) => result }
    matched match
      case Some((result, template)) =>
        val expanded     = expandTemplate(template, result)
        val patVars      = result.bindings.keySet ++ result.ellipsis.keySet
        val freeBindings = collectFreeBindings(template, patVars, literals.toSet, defEnv)
        (expanded, freeBindings)
      case None =>
        throw new EvalError(s"no matching macro rule for: ${Value.SList(form).display}")

  private def parseSyntaxRules(args: List[Value]): (List[String], List[(Value, Value)]) =
    args match
      case Value.SList(lits) :: rules =>
        val literals = lits.map {
          case Value.Symbol(s) => s
          case other           => throw new EvalError(s"literal must be symbol: ${other.display}")
        }
        val parsed = rules.map {
          case Value.SList(pattern :: template :: Nil) => (pattern, template)
          case other                                   => throw new EvalError(s"bad syntax rule: ${other.display}")
        }
        (literals, parsed)
      case _ => throw new EvalError("bad syntax-rules syntax")

  private def matchRule(
    pattern: Value,
    form: List[Value],
    literals: List[String]
  ): Option[MatchResult] =
    pattern match
      case Value.SList(patElems) if patElems.nonEmpty =>
        matchElems(patElems.tail, form.tail, literals, MatchResult(Map.empty, Map.empty))
      case _ => None

  @tailrec
  private def matchElems(
    patterns: List[Value],
    inputs: List[Value],
    literals: List[String],
    acc: MatchResult
  ): Option[MatchResult] =
    patterns match
      case Nil =>
        if inputs.isEmpty then Some(acc) else None
      case pat :: Value.Symbol("...") :: Nil =>
        pat match
          case Value.Symbol(name) if !literals.contains(name) =>
            Some(acc.copy(ellipsis = acc.ellipsis + (name -> inputs)))
          case _ => None
      case Value.Symbol(name) :: rest if !literals.contains(name) =>
        inputs match
          case input :: restIn =>
            matchElems(rest, restIn, literals, acc.copy(bindings = acc.bindings + (name -> input)))
          case Nil => None
      case Value.Symbol(name) :: rest if literals.contains(name) =>
        inputs match
          case Value.Symbol(n) :: restIn if n == name =>
            matchElems(rest, restIn, literals, acc)
          case _ => None
      case other :: rest =>
        inputs match
          case input :: restIn if input == other =>
            matchElems(rest, restIn, literals, acc)
          case _ => None

  private def expandTemplate(template: Value, result: MatchResult): Value =
    template match
      case Value.Symbol(name) =>
        result.bindings.get(name).getOrElse(template)
      case Value.SList(elems) =>
        Value.SList(expandListTemplate(elems, result))
      case other => other

  private def expandListTemplate(elems: List[Value], result: MatchResult): List[Value] =
    elems match
      case Nil => Nil
      case pat :: Value.Symbol("...") :: rest =>
        val varName = findEllipsisVar(pat, result)
        varName match
          case Some(name) =>
            val values = result.ellipsis(name)
            val expanded = values.map { v =>
              expandTemplate(pat, result.copy(bindings = result.bindings + (name -> v)))
            }
            expanded ++ expandListTemplate(rest, result)
          case None =>
            expandTemplate(pat, result) :: expandListTemplate(rest, result)
      case head :: rest =>
        expandTemplate(head, result) :: expandListTemplate(rest, result)

  private def findEllipsisVar(template: Value, result: MatchResult): Option[String] =
    template match
      case Value.Symbol(name) if result.ellipsis.contains(name) => Some(name)
      case Value.SList(elems) =>
        elems.collectFirst { e =>
          findEllipsisVar(e, result) match
            case Some(name) => name
        }
      case _ => None

  private def collectFreeBindings(
    template: Value,
    patVars: Set[String],
    literals: Set[String],
    defEnv: Env
  ): Map[String, Value] =
    val freeSyms = collectSymbols(template) -- patVars -- specialForms -- literals
    freeSyms.foldLeft(Map.empty[String, Value]) { (acc, name) =>
      try
        val v = defEnv.lookup(name)
        v match
          case _: Value.Macro => acc
          case _              => acc + (name -> v)
      catch case _: EvalError => acc
    }

  private def collectSymbols(v: Value): Set[String] = v match
    case Value.Symbol(name) => Set(name)
    case Value.SList(elems) => elems.flatMap(collectSymbols).toSet
    case _                  => Set.empty

  private val specialForms: Set[String] = Set(
    "quote",
    "if",
    "lambda",
    "define",
    "set!",
    "let",
    "begin",
    "cond",
    "and",
    "or",
    "define-syntax",
    "syntax-rules",
    "letrec",
    "letrec*",
    "case"
  )
