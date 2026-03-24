package ming

import scala.collection.mutable

object MacroExpander:

  private var gensymCounter = 0L

  private def gensym(base: String): String =
    gensymCounter += 1
    s"$base##$gensymCounter"

  private val specialFormNames = Set(
    "quote",
    "if",
    "define",
    "set!",
    "lambda",
    "let",
    "begin",
    "cond",
    "and",
    "or",
    "define-syntax",
    "syntax-rules"
  )

  def isMacro(name: String, env: Env): Boolean =
    env.lookupOpt(name).exists(_.isInstanceOf[SchemeVal.Macro])

  def collectPatVars(expr: Expr, literals: Set[String]): Set[String] =
    expr match
      case Expr.Symbol(n) if n != "_" && n != "..." && !literals.contains(n) => Set(n)
      case Expr.SList(elems) => elems.flatMap(e => collectPatVars(e, literals)).toSet
      case _                 => Set.empty

  def tryMatch(
    patElems: List[Expr],
    formElems: List[Expr],
    literals: Set[String]
  ): Option[Map[String, Either[Expr, List[Expr]]]] =
    patElems match
      case Nil =>
        if formElems.isEmpty then Some(Map.empty) else None
      case pat :: Expr.Symbol("...") :: rest if rest.isEmpty =>
        pat match
          case Expr.Symbol(n) if !literals.contains(n) && n != "_" =>
            Some(Map(n -> Right(formElems)))
          case _ => None
      case pat :: restPat =>
        if formElems.isEmpty then None
        else
          for
            b1 <- matchOne(pat, formElems.head, literals)
            b2 <- tryMatch(restPat, formElems.tail, literals)
          yield b1 ++ b2

  def matchOne(
    pattern: Expr,
    form: Expr,
    literals: Set[String]
  ): Option[Map[String, Either[Expr, List[Expr]]]] =
    pattern match
      case Expr.Symbol("_") => Some(Map.empty)
      case Expr.Symbol(name) if literals.contains(name) =>
        form match
          case Expr.Symbol(n) if n == name => Some(Map.empty)
          case _                           => None
      case Expr.Symbol(name) =>
        Some(Map(name -> Left(form)))
      case Expr.SList(patElems) =>
        form match
          case Expr.SList(formElems) => tryMatch(patElems, formElems, literals)
          case _                     => None
      case _ => None

  private def findEllipsisVars(
    tmpl: Expr,
    bindings: Map[String, Either[Expr, List[Expr]]],
    patVars: Set[String]
  ): Set[String] =
    tmpl match
      case Expr.Symbol(n) if patVars.contains(n) =>
        bindings.get(n) match
          case Some(Right(_)) => Set(n)
          case _              => Set.empty
      case Expr.SList(elems) =>
        elems.flatMap(e => findEllipsisVars(e, bindings, patVars)).toSet
      case _ => Set.empty

  private def expand(
    tmpl: Expr,
    bindings: Map[String, Either[Expr, List[Expr]]],
    renaming: mutable.Map[String, String],
    patVars: Set[String],
    literals: Set[String],
    macroName: String
  ): Expr =
    tmpl match
      case Expr.Symbol(n) if patVars.contains(n) =>
        bindings(n) match
          case Left(e)  => e
          case Right(_) => throw new EvalError(s"syntax-rules: ellipsis variable $n without ellipsis")
      case Expr.Symbol(n) if literals.contains(n) || specialFormNames.contains(n) || n == "..." || n == macroName =>
        tmpl
      case Expr.Symbol(n) =>
        Expr.Symbol(renaming.getOrElseUpdate(n, gensym(n)))
      case Expr.SList(elems) =>
        Expr.SList(expandList(elems, bindings, renaming, patVars, literals, macroName))
      case other => other

  private def expandList(
    elems: List[Expr],
    bindings: Map[String, Either[Expr, List[Expr]]],
    renaming: mutable.Map[String, String],
    patVars: Set[String],
    literals: Set[String],
    macroName: String
  ): List[Expr] =
    elems match
      case Nil => Nil
      case tmpl :: Expr.Symbol("...") :: rest =>
        val evars = findEllipsisVars(tmpl, bindings, patVars)
        if evars.isEmpty then
          expand(tmpl, bindings, renaming, patVars, literals, macroName) ::
            expandList(rest, bindings, renaming, patVars, literals, macroName)
        else
          val len = bindings(evars.head) match
            case Right(lst) => lst.length
            case _          => 0
          val expanded = (0 until len).toList.map { i =>
            val newBindings = bindings.map {
              case (k, Right(lst)) if evars.contains(k) =>
                (k, Left(lst(i)): Either[Expr, List[Expr]])
              case other => other
            }
            expand(tmpl, newBindings, renaming, patVars, literals, macroName)
          }
          expanded ++ expandList(rest, bindings, renaming, patVars, literals, macroName)
      case tmpl :: rest =>
        expand(tmpl, bindings, renaming, patVars, literals, macroName) ::
          expandList(rest, bindings, renaming, patVars, literals, macroName)

  /** Expand a macro application and evaluate the result. Returns Some(result) on match, None if no rule matched. */
  def expandAndEval(
    form: Expr,
    macroName: String,
    m: SchemeVal.Macro,
    useEnv: Env,
    evalFn: (Expr, Env) => SchemeVal
  ): SchemeVal =
    val formElems = form match
      case Expr.SList(elems) => elems
      case _                 => throw new EvalError("invalid macro application")

    val matched = m.rules.view.flatMap { case (pattern, template) =>
      val patElems = pattern match
        case Expr.SList(elems) => elems
        case _                 => throw new EvalError("syntax-rules: pattern must be a list")
      tryMatch(patElems.tail, formElems.tail, m.literals).map { bindings =>
        val patVars  = patElems.tail.flatMap(e => collectPatVars(e, m.literals)).toSet
        val renaming = mutable.Map[String, String]()
        val expanded = expand(template, bindings, renaming, patVars, m.literals, macroName)
        val evalEnv  = new Env(mutable.Map.empty, Some(useEnv))
        for (origName, gsName) <- renaming do m.defEnv.lookupOpt(origName).foreach(v => evalEnv.define(gsName, v))
        evalFn(expanded, evalEnv)
      }
    }.headOption

    matched.getOrElse(throw new EvalError(s"$macroName: no matching syntax rule"))
