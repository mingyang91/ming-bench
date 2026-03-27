package ming

private[ming] object InterpreterSequencePause:

  private val ExcludedForms = Set(
    "and",
    "begin",
    "case",
    "case-lambda",
    "cond",
    "define",
    "define-record-type",
    "define-syntax",
    "do",
    "guard",
    "if",
    "lambda",
    "let",
    "let*",
    "letrec",
    "letrec*",
    "or",
    "quasiquote",
    "quote",
    "set!",
    "syntax",
    "syntax-case",
    "with-syntax"
  )

  def shouldPauseAfter(expression: Expr, value: Value): Boolean =
    value == VoidValue && isCoroutineYieldExpression(expression)

  private def isCoroutineYieldExpression(expression: Expr): Boolean =
    expression match
      case ListExpr(List(SymbolExpr(name, _), callback), _) if isContinuationCapture(name) =>
        callback match
          case ListExpr(SymbolExpr("lambda", _) :: _ :: body, _) if body.length == 1 =>
            isUserApplication(body.head)
          case _ =>
            false
      case _ =>
        false

  private def isContinuationCapture(name: String): Boolean =
    name == "call/cc" || name == "call-with-current-continuation"

  private def isUserApplication(expression: Expr): Boolean =
    expression match
      case ListExpr(SymbolExpr(name, _) :: _, _) =>
        !ExcludedForms.contains(name)
      case ListExpr(_ :: _, _) =>
        true
      case _ =>
        false
