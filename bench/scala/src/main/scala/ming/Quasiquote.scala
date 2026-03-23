package ming

import SchemeValue.*

/** Quasiquote expansion: `expr, ,expr, ,@expr */
object Quasiquote:

  def eval(template: SchemeValue, env: Environment): SchemeValue =
    expand(template, env, depth = 0)

  private def expand(template: SchemeValue, env: Environment, depth: Int): SchemeValue =
    template match
      case ListVal(SymbolVal("unquote", _) :: inner :: Nil, _) =>
        if depth == 0 then Interpreter.eval(inner, env)
        else ListVal(List(SymbolVal("unquote"), expand(inner, env, depth - 1)))

      case ListVal(SymbolVal("quasiquote", _) :: inner :: Nil, _) =>
        ListVal(List(SymbolVal("quasiquote"), expand(inner, env, depth + 1)))

      case ListVal(elems, pos) =>
        expandList(elems, env, depth, pos)

      case MutablePairVal(cells) =>
        // Dotted pair in quasiquote template
        val car = expand(cells(0), env, depth)
        val cdr = expand(cells(1), env, depth)
        MutablePairVal(Array(car, cdr))

      case _ => template

  private def expandList(
    elems: List[SchemeValue],
    env: Environment,
    depth: Int,
    pos: Option[SourcePos]
  ): SchemeValue =
    val result = scala.collection.mutable.ListBuffer.empty[SchemeValue]
    var i      = 0
    while i < elems.length do
      elems(i) match
        case ListVal(SymbolVal("unquote-splicing", _) :: inner :: Nil, _) if depth == 0 =>
          val spliced = Interpreter.eval(inner, env)
          Builtins.toScalaList(spliced).foreach(result += _)
        case ListVal(SymbolVal("unquote-splicing", _) :: inner :: Nil, p) if depth > 0 =>
          result += ListVal(List(SymbolVal("unquote-splicing"), expand(inner, env, depth - 1)), p)
        case other =>
          result += expand(other, env, depth)
      i += 1
    ListVal(result.toList, pos)
