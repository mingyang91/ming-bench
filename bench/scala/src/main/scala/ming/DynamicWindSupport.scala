package ming

final private[ming] class WindFrame(
  val inThunk: Value,
  val outThunk: Value,
  val position: Position
)

private[ming] object WindFrame:

  def apply(inThunk: Value, outThunk: Value, position: Position): WindFrame =
    new WindFrame(inThunk, outThunk, position)
