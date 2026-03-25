package ming

final private[ming] case class ProcedureClause(
  params: List[String],
  restParam: Option[String],
  body: List[Expr]
):

  def matchesArity(argCount: Int): Boolean =
    restParam match
      case None    => params.length == argCount
      case Some(_) => argCount >= params.length
