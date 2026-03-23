package ming

import Value.*

/** syntax-case pattern matching on Values and template instantiation. */
object SyntaxCase:

  enum Binding:
    case Single(v: Value)
    case Ellipsis(vs: List[Value])

  /** Match a Value against a pattern (Expr), returning bindings on success. */
  def matchPattern(
    pattern: Expr,
    input: Value,
    literals: Set[String]
  ): Option[Map[String, Binding]] =
    pattern match
      case Expr.Sym("_", _) => Some(Map.empty)
      case Expr.Sym(name, _) if literals.contains(name) =>
        input match
          case SymbolVal(n) if n == name => Some(Map.empty)
          case _                         => None
      case Expr.Sym(name, _) => Some(Map(name -> Binding.Single(input)))
      case Expr.SList(pElems, _) =>
        input match
          case NilVal if pElems.isEmpty => Some(Map.empty)
          case _ =>
            valueToProperList(input) match
              case Some(elems) => matchPatternList(pElems, elems, literals)
              case None        => None
      case Expr.Num(n, _) =>
        input match
          case IntVal(n2) if n == n2 => Some(Map.empty)
          case _                     => None
      case Expr.Bool(b, _) =>
        input match
          case BoolVal(b2) if b == b2 => Some(Map.empty)
          case _                      => None
      case _ => None

  private def matchPatternList(
    patterns: List[Expr],
    inputs: List[Value],
    literals: Set[String]
  ): Option[Map[String, Binding]] =
    patterns match
      case Nil =>
        if inputs.isEmpty then Some(Map.empty) else None
      case pat :: Expr.Sym("...", _) :: Nil =>
        val matched = inputs.map(matchPattern(pat, _, literals))
        if matched.exists(_.isEmpty) then None
        else
          val allBindings = matched.map(_.get)
          val vars        = patternVars(pat, literals)
          val combined = vars.map { v =>
            v -> Binding.Ellipsis(allBindings.flatMap(_.get(v).map {
              case Binding.Single(e) => e
              case _                 => throw new EvalError("nested ellipsis not supported")
            }))
          }.toMap
          Some(combined)
      case pat :: restPat =>
        if inputs.isEmpty then None
        else
          for
            headB <- matchPattern(pat, inputs.head, literals)
            tailB <- matchPatternList(restPat, inputs.tail, literals)
          yield headB ++ tailB

  private def patternVars(pattern: Expr, literals: Set[String]): Set[String] =
    pattern match
      case Expr.Sym("...", _)                           => Set.empty
      case Expr.Sym("_", _)                             => Set.empty
      case Expr.Sym(name, _) if literals.contains(name) => Set.empty
      case Expr.Sym(name, _)                            => Set(name)
      case Expr.SList(elems, _) =>
        elems.flatMap(patternVars(_, literals)).toSet
      case _ => Set.empty

  /** Instantiate a syntax template with bindings, producing a Value. */
  def instantiateTemplate(
    template: Expr,
    bindings: Map[String, Binding],
    defEnv: Env,
    useEnv: Env
  ): Value =
    template match
      case Expr.Sym(name, _) =>
        bindings.get(name) match
          case Some(Binding.Single(v)) => v
          case Some(Binding.Ellipsis(_)) =>
            throw new EvalError(s"syntax: ellipsis variable $name used without ellipsis")
          case None => SymbolVal(name)
      case Expr.SList(elems, _) =>
        expandTemplateList(elems, bindings, defEnv, useEnv)
      case Expr.Num(n, _)    => IntVal(n)
      case Expr.Bool(b, _)   => BoolVal(b)
      case Expr.Str(s, _)    => StrVal(s.toCharArray)
      case Expr.Chr(c, _)    => CharVal(c)
      case Expr.Flt(d, _)    => FloatVal(d)
      case Expr.Rat(n, d, _) => Value.makeRational(n, d)

  private def expandTemplateList(
    elements: List[Expr],
    bindings: Map[String, Binding],
    defEnv: Env,
    useEnv: Env
  ): Value =
    val expanded = expandTemplateListToList(elements, bindings, defEnv, useEnv)
    expanded.foldRight(NilVal: Value)((v, acc) => Pair(v, acc))

  private def expandTemplateListToList(
    elements: List[Expr],
    bindings: Map[String, Binding],
    defEnv: Env,
    useEnv: Env
  ): List[Value] =
    elements match
      case Nil => Nil
      case elem :: Expr.Sym("...", _) :: rest =>
        splicedExpand(elem, bindings, defEnv, useEnv) ++
          expandTemplateListToList(rest, bindings, defEnv, useEnv)
      case Expr.Sym("quote", _) :: arg :: Nil =>
        List(SymbolVal("quote"), instantiateTemplate(arg, bindings, defEnv, useEnv))
      case elem :: rest =>
        instantiateTemplate(elem, bindings, defEnv, useEnv) ::
          expandTemplateListToList(rest, bindings, defEnv, useEnv)

  private def splicedExpand(
    template: Expr,
    bindings: Map[String, Binding],
    defEnv: Env,
    useEnv: Env
  ): List[Value] =
    val ellipsisVars = findEllipsisVars(template, bindings)
    if ellipsisVars.isEmpty then throw new EvalError("syntax: no ellipsis variable in spliced template")
    val len = bindings(ellipsisVars.head) match
      case Binding.Ellipsis(vs) => vs.length
      case _                    => throw new EvalError("not ellipsis")
    (0 until len).toList.map { i =>
      val singleBindings = bindings.map {
        case (name, Binding.Ellipsis(vs)) if ellipsisVars.contains(name) =>
          name -> Binding.Single(vs(i))
        case other => other
      }
      instantiateTemplate(template, singleBindings, defEnv, useEnv)
    }

  private def findEllipsisVars(
    template: Expr,
    bindings: Map[String, Binding]
  ): Set[String] =
    template match
      case Expr.Sym(name, _) =>
        bindings.get(name) match
          case Some(Binding.Ellipsis(_)) => Set(name)
          case _                         => Set.empty
      case Expr.SList(elems, _) =>
        elems.flatMap(findEllipsisVars(_, bindings)).toSet
      case _ => Set.empty

  /** Convert a Value to a proper list of Values, if possible. */
  def valueToProperList(v: Value): Option[List[Value]] =
    v match
      case NilVal => Some(Nil)
      case PairVal(cell) =>
        valueToProperList(cell.cdr).map(cell.car :: _)
      case _ => None

  /** Convert a Value back to an Expr for evaluation. */
  def valueToExpr(v: Value): Expr =
    v match
      case IntVal(n)         => Expr.Num(n)
      case RationalVal(n, d) => Expr.Rat(n, d)
      case FloatVal(d)       => Expr.Flt(d)
      case BoolVal(b)        => Expr.Bool(b)
      case SymbolVal(name)   => Expr.Sym(name)
      case StrVal(chars)     => Expr.Str(new String(chars))
      case CharVal(c)        => Expr.Chr(c)
      case NilVal            => Expr.SList(Nil)
      case PairVal(_) =>
        valueToProperList(v) match
          case Some(elems) => Expr.SList(elems.map(valueToExpr))
          case None        => throw new EvalError("syntax-case: cannot convert improper list to expression")
      case VoidVal => Expr.SList(List(Expr.Sym("void")))
      case _       => throw new EvalError(s"syntax-case: cannot convert ${v.display} to expression")
