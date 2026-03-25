package ming

import java.util.concurrent.atomic.AtomicLong

private[ming] object SchemeInterpreterDoSupport:

  import SchemeInterpreter.Expr

  final private case class DoBinding(
    name: String,
    initExpr: Expr,
    stepExpr: Option[Expr],
    pos: SourcePos
  )

  private val generatedNameCounter = new AtomicLong(0L)

  def expand(args: List[Expr], pos: SourcePos): Expr =
    args match
      case Expr.ListExpr(bindingExprs, _) :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val bindings   = readDoBindings(bindingExprs)
        val loopSymbol = Expr.Symbol(freshGeneratedName(), pos)
        val initBindings = bindings.map(binding =>
          Expr.ListExpr(List(Expr.Symbol(binding.name, binding.pos), binding.initExpr), binding.pos)
        )
        val nextArgs     = bindings.map(nextArgument)
        val recurExpr    = Expr.ListExpr(loopSymbol :: nextArgs, pos)
        val resultBranch = Expr.ListExpr(Expr.Symbol("begin", pos) :: resultExprs, pos)
        val bodyBranch   = Expr.ListExpr(Expr.Symbol("begin", pos) :: (body :+ recurExpr), pos)
        val ifExpr       = Expr.ListExpr(List(Expr.Symbol("if", pos), testExpr, resultBranch, bodyBranch), pos)
        Expr.ListExpr(
          List(
            Expr.Symbol("let", pos),
            loopSymbol,
            Expr.ListExpr(initBindings, pos),
            ifExpr
          ),
          pos
        )
      case _ =>
        throw EvalError.at(pos, "invalid do")

  private def readDoBindings(bindingExprs: List[Expr]): List[DoBinding] =
    bindingExprs.map {
      case Expr.ListExpr(List(Expr.Symbol(name, _), initExpr), bindingPos) =>
        DoBinding(name, initExpr, None, bindingPos)
      case Expr.ListExpr(List(Expr.Symbol(name, _), initExpr, stepExpr), bindingPos) =>
        DoBinding(name, initExpr, Some(stepExpr), bindingPos)
      case other =>
        throw EvalError.at(other.pos, s"invalid do binding: ${SchemeRendering.renderExpr(other)}")
    }

  private def nextArgument(binding: DoBinding): Expr =
    binding.stepExpr.getOrElse(Expr.Symbol(binding.name, binding.pos))

  private def freshGeneratedName(): String =
    s"__ming_do_${generatedNameCounter.incrementAndGet()}"
