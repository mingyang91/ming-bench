package ming

import Expr.*
import Macro.Binding

/** Template instantiation for syntax-rules macros — split from Macro. */
object MacroTemplate:
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
    "for-each",
    "=>",
    "quasiquote",
    "unquote",
    "unquote-splicing"
  )

  def instantiate(
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
      case DottedList(heads, tail, _) =>
        heads.flatMap(templateFreeSymbols(_, patVars)).toSet ++
          templateFreeSymbols(tail, patVars)
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
      case DottedList(heads, tail, pos) =>
        DottedList(
          expandListTemplate(heads, bindings, renaming),
          instantiateExpr(tail, bindings, renaming),
          pos
        )
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
    if ellipsisVars.isEmpty then
      throw new EvalError(
        "syntax-rules: no ellipsis variable in spliced template"
      )
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
      case DottedList(heads, tail, _) =>
        heads.flatMap(findEllipsisVars(_, bindings)).toSet ++
          findEllipsisVars(tail, bindings)
      case _ => Set.empty
