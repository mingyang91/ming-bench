package ming

import Expr.*

object Macro:
  private var gensymCounter = 0L

  private def gensym(name: String): String =
    gensymCounter += 1
    s"__macro_${name}_$gensymCounter"

  private val specialForms = Set(
    "if",
    "define",
    "set!",
    "quote",
    "lambda",
    "let",
    "begin",
    "cond",
    "and",
    "or",
    "not",
    "call/cc",
    "call-with-current-continuation",
    "apply",
    "define-syntax",
    "syntax-rules",
    "else",
    "guard",
    "define-record-type",
    "let*",
    "letrec",
    "letrec*",
    "case",
    "do",
    "raise",
    "with-exception-handler",
    "dynamic-wind",
    "call-with-values",
    "values",
    "syntax-case",
    "syntax",
    "with-syntax",
    "map",
    "for-each"
  )

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
          instantiate(template, bindings, defEnv, useEnv)
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
      case _ => Set.empty

  private def instantiate(
    template: Expr,
    bindings: Map[String, Binding],
    defEnv: Env,
    useEnv: Env
  ): Expr =
    val patVarNames = bindings.keySet
    val freeSyms    = templateFreeSymbols(template, patVarNames)
    val renaming = freeSyms
      .filterNot(specialForms.contains)
      .map(name => name -> gensym(name))
      .toMap
    renaming.foreach { (origName, gsName) =>
      try
        val v = defEnv.lookup(origName)
        useEnv.define(gsName, v)
      catch case _: EvalError => ()
    }
    instantiateExpr(template, bindings, renaming)

  private def templateFreeSymbols(
    template: Expr,
    patVars: Set[String]
  ): Set[String] =
    template match
      case Sym("...", _)                          => Set.empty
      case Sym(name, _) if patVars.contains(name) => Set.empty
      case Sym(name, _)                           => Set(name)
      case SList(elements, _) =>
        elements.flatMap(templateFreeSymbols(_, patVars)).toSet
      case _ => Set.empty

  private def instantiateExpr(
    template: Expr,
    bindings: Map[String, Binding],
    renaming: Map[String, String]
  ): Expr =
    template match
      case Sym(name, pos) =>
        bindings.get(name) match
          case Some(Binding.Single(expr)) => expr
          case Some(Binding.Ellipsis(_)) =>
            throw new EvalError(
              s"syntax-rules: ellipsis variable $name used without ellipsis"
            )
          case None =>
            renaming.get(name) match
              case Some(gs) => Sym(gs, pos)
              case None     => template
      case SList(elements, pos) =>
        SList(expandListTemplate(elements, bindings, renaming), pos)
      case _ => template

  private def expandListTemplate(
    elements: List[Expr],
    bindings: Map[String, Binding],
    renaming: Map[String, String]
  ): List[Expr] =
    elements match
      case Nil => Nil
      case elem :: Sym("...", _) :: rest =>
        splicedExpand(elem, bindings, renaming) ++
          expandListTemplate(rest, bindings, renaming)
      case elem :: rest =>
        instantiateExpr(elem, bindings, renaming) ::
          expandListTemplate(rest, bindings, renaming)

  private def splicedExpand(
    template: Expr,
    bindings: Map[String, Binding],
    renaming: Map[String, String]
  ): List[Expr] =
    val ellipsisVars = findEllipsisVars(template, bindings)
    if ellipsisVars.isEmpty then throw new EvalError("syntax-rules: no ellipsis variable in spliced template")
    val len = bindings(ellipsisVars.head) match
      case Binding.Ellipsis(exprs) => exprs.length
      case _                       => throw new EvalError("not ellipsis")
    (0 until len).toList.map { i =>
      val singleBindings = bindings.map {
        case (name, Binding.Ellipsis(exprs)) if ellipsisVars.contains(name) =>
          name -> Binding.Single(exprs(i))
        case other => other
      }
      instantiateExpr(template, singleBindings, renaming)
    }

  private def findEllipsisVars(
    template: Expr,
    bindings: Map[String, Binding]
  ): Set[String] =
    template match
      case Sym(name, _) =>
        bindings.get(name) match
          case Some(Binding.Ellipsis(_)) => Set(name)
          case _                         => Set.empty
      case SList(elements, _) =>
        elements.flatMap(findEllipsisVars(_, bindings)).toSet
      case _ => Set.empty
