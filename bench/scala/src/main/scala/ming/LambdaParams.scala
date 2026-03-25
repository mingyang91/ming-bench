package ming

final private[ming] case class LambdaParams(required: List[String], rest: Option[String])

private[ming] object LambdaParams:

  def fixed(params: List[String]): LambdaParams =
    LambdaParams(params, None)
