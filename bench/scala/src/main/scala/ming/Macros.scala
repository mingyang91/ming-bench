package ming

import scala.annotation.tailrec

private[ming] object MacroExpander:

  def define(args: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: transformer :: Nil =>
        macros.define(name, MacroPatternMatcher.parseTransformer(transformer, env, pos))
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "define-syntax expects a name and transformer")

  def expand(expr: Expr, macros: MacroState): Expr =
    @tailrec
    def loop(current: Expr): Expr =
      current match
        case Expr.ListExpr(Expr.Symbol(name, _) :: _, pos) =>
          macros.lookup(name) match
            case Some(macroDef) =>
              loop(MacroPatternMatcher.expandMacro(name, current, macroDef, macros, pos))

            case None =>
              current

        case _ =>
          current

    loop(expr)
