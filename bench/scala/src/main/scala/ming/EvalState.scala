package ming

final case class EvalState(env: Env, outputRev: List[String]):

  def withEnv(nextEnv: Env): EvalState =
    copy(env = nextEnv)

  def appendOutput(chunk: String): EvalState =
    if chunk.isEmpty then this else copy(outputRev = chunk :: outputRev)

  def renderedOutput: String =
    outputRev.reverse.mkString

object EvalState:

  val empty: EvalState = EvalState(Env.empty, Nil)
