package ming

import Expr.*

object Macro:

  enum Binding:
    case Single(expr: Expr)
    case Ellipsis(exprs: List[Expr])

  def parseSyntaxRules(
    args: List[Expr],
    pos: Option[Pos]
  ): (List[String], List[(List[Expr], Expr)]) =
    args match
      case SList(literals, _) :: rules =>
        val litNames = literals.map {
          case Sym(name, _) => name
          case _            => throw new EvalError("syntax-rules: literal must be identifier")
        }
        val macroRules = rules.map {
          case SList(SList(elements, _) :: template :: Nil, _) =>
            (elements.tail, template)
          case SList(DottedList(elements, tail, dpos) :: template :: Nil, _) =>
            (List(DottedList(elements.tail, tail, dpos)), template)
          case _ => throw new EvalError("syntax-rules: bad rule")
        }
        (litNames, macroRules)
      case _ => throw new EvalError("syntax-rules: bad syntax")

  def expand(
    macroName: String,
    args: List[Expr],
    literals: List[String],
    rules: List[(List[Expr], Expr)],
    defEnv: Env,
    useEnv: Env,
    pos: Option[Pos]
  ): Expr =
    rules.iterator
      .flatMap { (pattern, template) =>
        matchPatternList(pattern, args, literals).map { bindings =>
          MacroTemplate.instantiate(template, bindings, defEnv, useEnv)
        }
      }
      .nextOption()
      .getOrElse(
        throw new EvalError(s"$macroName: no matching pattern")
      )

  private def matchPatternList(
    pattern: List[Expr],
    input: List[Expr],
    literals: List[String]
  ): Option[Map[String, Binding]] =
    pattern match
      case Nil =>
        if input.isEmpty then Some(Map.empty) else None

      case pat :: Sym("...", _) :: Nil =>
        val matched = input.map(matchSingle(pat, _, literals))
        if matched.exists(_.isEmpty) then None
        else
          val allBindings = matched.map(_.get)
          val vars        = patternVarsExpr(pat, literals)
          val combined = vars.map { v =>
            v -> Binding.Ellipsis(allBindings.flatMap(_.get(v).map {
              case Binding.Single(e) => e
              case _                 => throw new EvalError("nested ellipsis not supported")
            }))
          }.toMap
          Some(combined)

      case (dl @ DottedList(pHeads, pTail, _)) :: Nil =>
        if input.length < pHeads.length then None
        else
          val (headInput, tailInput) = input.splitAt(pHeads.length)
          for
            headBindings <- matchPatternList(pHeads, headInput, literals)
            tailBindings <- matchSingle(pTail, SList(tailInput, None), literals)
          yield headBindings ++ tailBindings

      case pat :: restPat =>
        if input.isEmpty then None
        else
          for
            headBindings <- matchSingle(pat, input.head, literals)
            tailBindings <- matchPatternList(restPat, input.tail, literals)
          yield headBindings ++ tailBindings

  private def matchSingle(
    pattern: Expr,
    input: Expr,
    literals: List[String]
  ): Option[Map[String, Binding]] =
    pattern match
      case Sym("_", _) => Some(Map.empty)
      case Sym(name, _) if literals.contains(name) =>
        input match
          case Sym(iname, _) if iname == name => Some(Map.empty)
          case _                              => None
      case Sym(name, _) => Some(Map(name -> Binding.Single(input)))
      case SList(pElems, _) =>
        input match
          case SList(iElems, _) => matchPatternList(pElems, iElems, literals)
          case _                => None
      case DottedList(pHeads, pTail, _) =>
        input match
          case SList(iElems, _) if iElems.length >= pHeads.length =>
            val (headInput, tailInput) = iElems.splitAt(pHeads.length)
            val tailExpr               = SList(tailInput, None)
            for
              headBindings <- matchPatternList(pHeads, headInput, literals)
              tailBindings <- matchSingle(pTail, tailExpr, literals)
            yield headBindings ++ tailBindings
          case DottedList(iHeads, iTail, _) if iHeads.length >= pHeads.length =>
            val (headInput, extraInput) = iHeads.splitAt(pHeads.length)
            val tailExpr =
              if extraInput.isEmpty then iTail
              else DottedList(extraInput, iTail, None)
            for
              headBindings <- matchPatternList(pHeads, headInput, literals)
              tailBindings <- matchSingle(pTail, tailExpr, literals)
            yield headBindings ++ tailBindings
          case _ => None
      case Num(n, _) =>
        input match
          case Num(n2, _) if n == n2 => Some(Map.empty)
          case _                     => None
      case Bool(b, _) =>
        input match
          case Bool(b2, _) if b == b2 => Some(Map.empty)
          case _                      => None
      case _ => None

  private def patternVarsExpr(expr: Expr, literals: List[String]): Set[String] =
    expr match
      case Sym("...", _)                           => Set.empty
      case Sym("_", _)                             => Set.empty
      case Sym(name, _) if literals.contains(name) => Set.empty
      case Sym(name, _)                            => Set(name)
      case SList(elements, _) =>
        elements.flatMap(patternVarsExpr(_, literals)).toSet
      case DottedList(heads, tail, _) =>
        heads.flatMap(patternVarsExpr(_, literals)).toSet ++ patternVarsExpr(tail, literals)
      case _ => Set.empty
