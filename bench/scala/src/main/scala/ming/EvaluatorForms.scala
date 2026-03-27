package ming

final private[ming] case class LetBinding(name: String, valueExpr: Expr)
final private[ming] case class DoBinding(name: String, initExpr: Expr, stepExpr: Option[Expr])

final private[ming] case class ParameterSpec(params: List[String], restParam: Option[String])

private[ming] object EvaluatorForms:

  def parseLetBindings(bindings: List[Expr]): List[LetBinding] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        LetBinding(name, valueExpr)
      case invalid =>
        throw EvalError.at(invalid.pos, "invalid let binding")
    }

  def parseParameterSpec(expr: Expr): ParameterSpec =
    expr match
      case Expr.ListExpr(params, _) => parseParameterSpec(params)
      case Expr.Symbol(name, _)     => ParameterSpec(Nil, Some(name))
      case other                    => throw EvalError.at(other.pos, "parameter list must be a list or symbol")

  def parseParameterSpec(params: List[Expr]): ParameterSpec =
    @annotation.tailrec
    def loop(remaining: List[Expr], fixed: List[String]): ParameterSpec =
      remaining match
        case Nil =>
          ParameterSpec(fixed.reverse, None)
        case Expr.Symbol(".", _) :: Expr.Symbol(name, _) :: Nil =>
          ParameterSpec(fixed.reverse, Some(name))
        case Expr.Symbol(".", dotPos) :: _ =>
          throw EvalError.at(dotPos, "dot must appear before a single rest parameter")
        case Expr.Symbol(name, _) :: tail =>
          loop(tail, name :: fixed)
        case other :: _ =>
          throw EvalError.at(other.pos, "parameter names must be symbols")

    loop(params, Nil)

  def parseDoBindings(bindings: List[Expr]): List[DoBinding] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
        DoBinding(name, initExpr, None)
      case Expr.ListExpr(Expr.Symbol(name, _) :: initExpr :: stepExpr :: Nil, _) =>
        DoBinding(name, initExpr, Some(stepExpr))
      case invalid =>
        throw EvalError.at(invalid.pos, "invalid do binding")
    }
