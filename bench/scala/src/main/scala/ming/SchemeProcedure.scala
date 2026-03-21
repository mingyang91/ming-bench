package ming

sealed trait SchemeProcedure:
  def displayName: String

object SchemeProcedure:

  final case class Builtin(name: String) extends SchemeProcedure:
    def displayName: String = name

  final case class Lambda(
    name: Option[String],
    parameters: List[String],
    body: List[Expr],
    environment: Environment
  ) extends SchemeProcedure:

    def displayName: String =
      name.getOrElse("lambda")

  def named(procedure: SchemeProcedure, name: String): SchemeProcedure = procedure match
    case lambda: Lambda if lambda.name.isEmpty => lambda.copy(name = Some(name))
    case _                                     => procedure
