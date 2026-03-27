package ming

import scala.util.DynamicVariable

private[ming] object MacroRuntime:

  private val definitionEnvState = DynamicVariable(Option.empty[Environment])

  def withDefinitionEnv[A](definitionEnv: Environment)(body: => A): A =
    definitionEnvState.withValue(Some(definitionEnv))(body)

  def definitionEnv(position: Position): Environment =
    definitionEnvState.value.getOrElse(
      SchemeFailure.raise("syntax used outside macro transformer", position)
    )
