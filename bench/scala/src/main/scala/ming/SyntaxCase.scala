package ming

import SchemeValue.*

/** syntax-case macro system: pattern matching, template expansion, transformer invocation. */
object SyntaxCase:

  sealed trait Binding
  case class Single(value: SchemeValue)         extends Binding
  case class Spliced(values: List[SchemeValue]) extends Binding

  val BindingsKey = "%syntax-bindings%"

  /** Expand a procedure-based macro by calling its transformer. */
  def expandProcMacro(m: SchemeProcMacro, form: SchemeValue): SchemeValue =
    val bodyExpr =
      if m.transformer.body.length == 1 then m.transformer.body.head
      else SchemeList(SchemeSymbol("begin") :: m.transformer.body)
    val callEnv = m.transformer.closure
      .extend(m.transformer.params, List(form))
      .extend(BindingsKey, SchemeSyntaxBindings(Map.empty, m.defEnv))
    Evaluator.eval(bodyExpr, callEnv)

  /** Match form against syntax-case clauses, return (bindings, body). */
  def matchClauses(
    form: SchemeValue,
    literals: List[String],
    clauses: List[SchemeValue]
  ): (Map[String, Binding], SchemeValue) =
    clauses match
      case Nil => throw new EvalError("syntax-case: no matching pattern")
      case SchemeList(elems) :: rest =>
        elems match
          case pat :: body :: Nil =>
            matchPattern(pat, form, literals) match
              case Some(bindings) => (bindings, body)
              case None           => matchClauses(form, literals, rest)
          case pat :: fender :: body :: Nil =>
            matchPattern(pat, form, literals) match
              case Some(bindings) => (bindings, body)
              case None           => matchClauses(form, literals, rest)
          case _ => throw new EvalError("syntax-case: bad clause")
      case other :: _ =>
        throw new EvalError(s"syntax-case: bad clause: ${other.display}")

  /** Match a single pattern against a form. */
  private def matchPattern(
    pattern: SchemeValue,
    form: SchemeValue,
    literals: List[String]
  ): Option[Map[String, Binding]] =
    val formElems = toList(form)
    pattern match
      case SchemeList(pelems) if pelems.nonEmpty =>
        matchElems(pelems, formElems, literals, Map.empty)
      case _ => None

  private def toList(v: SchemeValue): List[SchemeValue] = v match
    case SchemeList(es) => es
    case p: SchemePair  => Builtins.asList(p)
    case _              => List(v)

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
        case SchemeList(ielems) => matchElems(pelems, ielems, literals, bindings)
        case _                  => None
    case _ =>
      if pat == input then Some(bindings) else None

  /** Expand a syntax-quote template with bindings and hygiene. */
  def expandTemplate(
    tmpl: SchemeValue,
    bindings: Map[String, Binding],
    defEnv: Env
  ): SchemeValue =
    val pvars = bindings.keySet
    doExpand(tmpl, bindings, defEnv, pvars)

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
        case Some(_: SchemeMacro)     => tmpl
        case Some(_: SchemeProcMacro) => tmpl
        case Some(_)                  => new SchemeResolvedSymbol(n, defEnv)
        case None                     => tmpl
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
        "call-with-current-continuation" | "define-syntax" | "syntax-rules" | "syntax-case" | "syntax-quote" |
        "with-syntax" =>
      true
    case _ => false
