package ming

final case class EvalState(
  env: Env,
  outputRev: List[String],
  strings: Map[Int, String],
  nextStringId: Int
):

  def withEnv(nextEnv: Env): EvalState =
    copy(env = nextEnv)

  def appendOutput(chunk: String): EvalState =
    if chunk.isEmpty then this else copy(outputRev = chunk :: outputRev)

  def renderedOutput: String =
    outputRev.reverse.mkString

  def readString(value: StringStorage): String = value match
    case StringStorage.Immutable(text) =>
      text
    case StringStorage.Mutable(id) =>
      strings.getOrElse(id, throw new IllegalStateException(s"unknown string id: $id"))

  def allocateMutableString(value: String): (EvalState, Value) =
    val id = nextStringId
    (
      copy(strings = strings.updated(id, value), nextStringId = id + 1),
      Value.Str(StringStorage.Mutable(id))
    )

  def writeMutableString(id: Int, value: String): EvalState =
    require(strings.contains(id), s"unknown string id: $id")
    copy(strings = strings.updated(id, value))

object EvalState:

  val empty: EvalState = EvalState(Env.empty, Nil, Map.empty, 0)
