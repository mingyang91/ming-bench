package ming

import SchemeValue.*

/** Hygienic macro expansion for syntax-rules. */
object Macros:

  def expand(m: SchemeMacro, form: List[SchemeValue]): SchemeValue =
    tryRules(m.literals, m.rules, m.defEnv, form)

  def parseSyntaxRules(form: SchemeValue, defEnv: Env): SchemeMacro =
    form match
      case SchemeList(SchemeSymbol("syntax-rules") :: SchemeList(lits) :: rules) =>
        val litNames = lits.map {
          case SchemeSymbol(n) => n
          case other =>
            throw new EvalError(s"syntax-rules: bad literal: ${other.display}")
        }
        val parsed = rules.map {
          case SchemeList(pat :: tmpl :: Nil) => (pat, tmpl)
          case other =>
            throw new EvalError(s"syntax-rules: bad rule: ${other.display}")
        }
        SchemeMacro(litNames, parsed, defEnv)
      case _ => throw new EvalError("bad syntax-rules")

  // --- Pattern matching ---

  sealed private trait Binding
  private case class Single(value: SchemeValue)         extends Binding
  private case class Spliced(values: List[SchemeValue]) extends Binding

  @scala.annotation.tailrec
  private def tryRules(
    literals: List[String],
    rules: List[(SchemeValue, SchemeValue)],
    defEnv: Env,
    form: List[SchemeValue]
  ): SchemeValue = rules match
    case Nil => throw new EvalError("no matching syntax-rules pattern")
    case (pattern, template) :: rest =>
      matchPattern(pattern, form, literals) match
        case Some(bindings) => doExpand(template, bindings, defEnv, bindings.keySet)
        case None           => tryRules(literals, rest, defEnv, form)

  private def matchPattern(
    pattern: SchemeValue,
    form: List[SchemeValue],
    literals: List[String]
  ): Option[Map[String, Binding]] = pattern match
    case SchemeList(elems) if elems.nonEmpty =>
      matchElems(elems.tail, form.tail, literals, Map.empty)
    case _ => None

  @scala.annotation.tailrec
  private def matchElems(
    pats: List[SchemeValue],
    inputs: List[SchemeValue],
    literals: List[String],
    bindings: Map[String, Binding]
  ): Option[Map[String, Binding]] = pats match
    case Nil =>
      if inputs.isEmpty then Some(bindings) else None
    case SchemeSymbol(name) :: SchemeSymbol("...") :: Nil if !literals.contains(name) =>
      Some(bindings + (name -> Spliced(inputs)))
    case pat :: rest =>
      inputs match
        case Nil => None
        case input :: restIn =>
          matchOne(pat, input, literals, bindings) match
            case Some(b) => matchElems(rest, restIn, literals, b)
            case None    => None

  private def matchOne(
    pat: SchemeValue,
    input: SchemeValue,
    literals: List[String],
    bindings: Map[String, Binding]
  ): Option[Map[String, Binding]] = pat match
    case SchemeSymbol(n) if literals.contains(n) =>
      input match
        case SchemeSymbol(m) if m == n => Some(bindings)
        case _                         => None
    case SchemeSymbol("_") => Some(bindings)
    case SchemeSymbol(n)   => Some(bindings + (n -> Single(input)))
    case SchemeList(pelems) =>
      input match
        case SchemeList(ielems) =>
          matchElems(pelems, ielems, literals, bindings)
        case _ => None
    case _ =>
      if pat == input then Some(bindings) else None

  // --- Template expansion ---

  private def doExpand(
    tmpl: SchemeValue,
    bindings: Map[String, Binding],
    defEnv: Env,
    pvars: Set[String]
  ): SchemeValue = tmpl match
    case SchemeSymbol(n) if bindings.contains(n) =>
      bindings(n) match
        case Single(v)  => v
        case Spliced(_) => throw new EvalError(s"missing ... for: $n")
    case SchemeSymbol(n) if !isKeyword(n) && !pvars.contains(n) =>
      defEnv.get(n) match
        case Some(_: SchemeMacro) => tmpl
        case Some(_)              => new SchemeResolvedSymbol(n, defEnv)
        case None                 => tmpl
    case SchemeList(elems) =>
      SchemeList(expandElems(elems, bindings, defEnv, pvars))
    case _ => tmpl

  private def expandElems(
    elems: List[SchemeValue],
    bindings: Map[String, Binding],
    defEnv: Env,
    pvars: Set[String]
  ): List[SchemeValue] = elems match
    case Nil => Nil
    case e :: SchemeSymbol("...") :: rest =>
      spliceSub(e, bindings, defEnv, pvars) ++
        expandElems(rest, bindings, defEnv, pvars)
    case e :: rest =>
      doExpand(e, bindings, defEnv, pvars) ::
        expandElems(rest, bindings, defEnv, pvars)

  private def spliceSub(
    tmpl: SchemeValue,
    bindings: Map[String, Binding],
    defEnv: Env,
    pvars: Set[String]
  ): List[SchemeValue] =
    val evars = findSplicedVars(tmpl, bindings)
    if evars.isEmpty then Nil
    else
      val n = bindings(evars.head) match
        case Spliced(vs) => vs.length
        case _           => 0
      (0 until n).toList.map { i =>
        val oneBindings = bindings.map {
          case (k, Spliced(vs)) if evars.contains(k) => (k, Single(vs(i)))
          case kv                                    => kv
        }
        doExpand(tmpl, oneBindings, defEnv, pvars)
      }

  private def findSplicedVars(
    tmpl: SchemeValue,
    bindings: Map[String, Binding]
  ): Set[String] = tmpl match
    case SchemeSymbol(n) =>
      bindings.get(n) match
        case Some(Spliced(_)) => Set(n)
        case _                => Set.empty
    case SchemeList(es) =>
      es.flatMap(findSplicedVars(_, bindings)).toSet
    case _ => Set.empty

  private def isKeyword(n: String): Boolean = n match
    case "define" | "if" | "quote" | "lambda" | "and" | "or" | "not" | "let" | "begin" | "cond" | "set!" | "call/cc" |
        "call-with-current-continuation" | "define-syntax" | "syntax-rules" =>
      true
    case _ => false
