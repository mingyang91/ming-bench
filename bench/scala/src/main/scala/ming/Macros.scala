package ming

import java.util.concurrent.atomic.AtomicLong

/** Hygienic macro expansion for syntax-rules. */
object Macros:

  private val counter = AtomicLong(0)

  private def gensym(base: String): String =
    s"__m_${base}_${counter.getAndIncrement()}__"

  private val specialForms: Set[String] = Set(
    "define",
    "if",
    "quote",
    "lambda",
    "let",
    "set!",
    "begin",
    "cond",
    "and",
    "or",
    "not",
    "display",
    "write",
    "newline",
    "call/cc",
    "call-with-current-continuation",
    "define-syntax",
    "syntax-rules",
    "else",
    "apply",
    "let*",
    "letrec"
  )

  sealed trait Binding
  case class Single(value: Value)          extends Binding
  case class Ellipsis(values: List[Value]) extends Binding

  def parseSyntaxRules(form: Value, defEnv: Env): Value.MacroVal =
    Evaluator.toList(form) match
      case Value.Symbol("syntax-rules", _) :: litsVal :: ruleForms =>
        val lits = Evaluator.toList(litsVal).map {
          case Value.Symbol(n, _) => n
          case _ =>
            throw new EvalError("syntax-rules: literals must be identifiers")
        }
        val rules = ruleForms.map { r =>
          Evaluator.toList(r) match
            case pat :: tmpl :: Nil => (pat, tmpl)
            case _                  => throw new EvalError("syntax-rules: bad rule")
        }
        Value.MacroVal(rules, lits, () => defEnv)
      case _ =>
        throw new EvalError("expected (syntax-rules ...)")

  /** Expand macro. Returns (expanded-form, env-bindings-to-inject). */
  def expand(
    m: Value.MacroVal,
    input: List[Value]
  ): (Value, Map[String, Value]) =
    val defEnv = m.defEnv()
    m.rules.iterator
      .map { case (pat, tmpl) =>
        matchRule(pat, input, m.literals)
          .map(binds => instantiate(tmpl, binds, defEnv))
      }
      .collectFirst { case Some(result) => result }
      .getOrElse(throw new EvalError("no matching syntax-rules pattern"))

  private def matchRule(
    pattern: Value,
    input: List[Value],
    literals: List[String]
  ): Option[Map[String, Binding]] =
    matchElems(
      Evaluator.toList(pattern).tail,
      input.tail,
      literals,
      Map.empty
    )

  private def matchElems(
    pats: List[Value],
    inps: List[Value],
    lits: List[String],
    binds: Map[String, Binding]
  ): Option[Map[String, Binding]] =
    pats match
      case Nil =>
        if inps.isEmpty then Some(binds) else None
      case p :: Value.Symbol("...", _) :: rest if rest.isEmpty =>
        p match
          case Value.Symbol(n, _) if !lits.contains(n) =>
            Some(binds + (n -> Ellipsis(inps)))
          case _ => None
      case p :: tail =>
        inps match
          case Nil => None
          case i :: iTail =>
            matchOne(p, i, lits, binds)
              .flatMap(b => matchElems(tail, iTail, lits, b))

  private def matchOne(
    pat: Value,
    inp: Value,
    lits: List[String],
    binds: Map[String, Binding]
  ): Option[Map[String, Binding]] =
    pat match
      case Value.Symbol(n, _) if lits.contains(n) =>
        inp match
          case Value.Symbol(m, _) if m == n => Some(binds)
          case _                            => None
      case Value.Symbol("_", _) => Some(binds)
      case Value.Symbol(n, _) =>
        Some(binds + (n -> Single(inp)))
      case Value.NilVal =>
        if inp == Value.NilVal then Some(binds) else None
      case Value.PairVal(_, _, _) =>
        inp match
          case Value.PairVal(_, _, _) =>
            matchElems(
              Evaluator.toList(pat),
              Evaluator.toList(inp),
              lits,
              binds
            )
          case _ => None
      case _ =>
        if valEq(pat, inp) then Some(binds) else None

  private def valEq(a: Value, b: Value): Boolean = (a, b) match
    case (Value.IntVal(x), Value.IntVal(y))       => x == y
    case (Value.BoolVal(x), Value.BoolVal(y))     => x == y
    case (Value.StringVal(x), Value.StringVal(y)) => x == y
    case (Value.Symbol(x, _), Value.Symbol(y, _)) => x == y
    case (Value.NilVal, Value.NilVal)             => true
    case _                                        => false

  private def instantiate(
    tmpl: Value,
    binds: Map[String, Binding],
    defEnv: Env
  ): (Value, Map[String, Value]) =
    val patVars  = binds.keySet
    val freeSyms = collectFree(tmpl, patVars)
    val (gsMap, envBinds) = freeSyms.foldLeft(
      (Map.empty[String, String], Map.empty[String, Value])
    ) { case ((gm, eb), name) =>
      if specialForms.contains(name) then (gm, eb)
      else
        val gs = gensym(name)
        safeLookup(defEnv, name) match
          case Some(v) => (gm + (name -> gs), eb + (gs -> v))
          case None    => (gm + (name -> gs), eb)
    }
    (subst(tmpl, binds, gsMap), envBinds)

  private def safeLookup(env: Env, name: String): Option[Value] =
    try Some(env.lookup(name))
    catch case _: EvalError => None

  private def collectFree(
    tmpl: Value,
    patVars: Set[String]
  ): Set[String] =
    tmpl match
      case Value.Symbol(n, _) if !patVars.contains(n) && n != "..." =>
        Set(n)
      case Value.PairVal(_, _, _) =>
        Evaluator.toList(tmpl).foldLeft(Set.empty[String]) { (a, v) =>
          a ++ collectFree(v, patVars)
        }
      case _ => Set.empty

  private def subst(
    tmpl: Value,
    binds: Map[String, Binding],
    gsMap: Map[String, String]
  ): Value =
    tmpl match
      case Value.Symbol(name, pos) =>
        binds.get(name) match
          case Some(Single(v)) => v
          case Some(_: Ellipsis) =>
            throw new EvalError(s"ellipsis variable without ...")
          case None =>
            gsMap.get(name) match
              case Some(gs) => Value.Symbol(gs, pos)
              case None     => tmpl
      case Value.PairVal(_, _, _) =>
        listToValue(substList(Evaluator.toList(tmpl), binds, gsMap))
      case _ => tmpl

  private def substList(
    elems: List[Value],
    binds: Map[String, Binding],
    gsMap: Map[String, String]
  ): List[Value] =
    elems match
      case Nil => Nil
      case e :: Value.Symbol("...", _) :: rest =>
        spliceEllipsis(e, binds, gsMap) ++
          substList(rest, binds, gsMap)
      case e :: rest =>
        subst(e, binds, gsMap) :: substList(rest, binds, gsMap)

  private def spliceEllipsis(
    tmpl: Value,
    binds: Map[String, Binding],
    gsMap: Map[String, String]
  ): List[Value] =
    val used = findEllipsisVars(tmpl, binds)
    if used.isEmpty then Nil
    else
      val count = used.values.head match
        case Ellipsis(vs) => vs.length
        case _            => 0
      (0 until count).toList.map { i =>
        val iterBinds = binds.map {
          case (k, Ellipsis(vs)) if used.contains(k) =>
            k -> Single(if i < vs.length then vs(i) else Value.NilVal)
          case other => other
        }
        subst(tmpl, iterBinds, gsMap)
      }

  private def findEllipsisVars(
    tmpl: Value,
    binds: Map[String, Binding]
  ): Map[String, Binding] =
    tmpl match
      case Value.Symbol(n, _) =>
        binds.get(n) match
          case Some(e: Ellipsis) => Map(n -> e)
          case _                 => Map.empty
      case Value.PairVal(_, _, _) =>
        Evaluator.toList(tmpl).foldLeft(Map.empty[String, Binding]) { (acc, v) =>
          acc ++ findEllipsisVars(v, binds)
        }
      case _ => Map.empty

  private def listToValue(elems: List[Value]): Value =
    elems.foldRight(Value.NilVal: Value)(Value.PairVal(_, _))
