package ming

private[ming] enum Value:
  case IntegerValue(value: Int)
  case BooleanValue(value: Boolean)
  case StringValue(value: String)
  case SymbolValue(name: String)
  case EmptyListValue
  case PairValue(head: Value, tail: Value)
  case BuiltinValue(name: String, applyTo: List[Value] => Value)
  case ClosureValue(parameters: List[String], body: List[Expr], environment: Environment)
  case VoidValue

final private[ming] case class Binding(force: () => Value)

private[ming] object Binding:

  def delay(value: => Value): Binding =
    lazy val cached: Value = value
    Binding(() => cached)

  def eager(value: Value): Binding =
    delay(value)

final private[ming] case class Environment(
  bindings: Map[String, Binding],
  parent: Option[Environment]
):

  def lookup(name: String): Option[Value] =
    bindings.get(name).map(_.force()).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, binding: Binding): Environment =
    copy(bindings = bindings.updated(name, binding))

  def extend(entries: List[(String, Binding)]): Environment =
    Environment(entries.toMap, Some(this))

private[ming] object Environment:
  val empty: Environment = Environment(Map.empty, None)

final private[ming] case class EvalOutcome(
  value: Value,
  environment: Environment
)

final private[ming] case class ArgumentOutcome(
  values: List[Value],
  environment: Environment
)
