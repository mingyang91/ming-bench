package ming

import scala.collection.mutable

sealed private[ming] trait Value:
  def render: String
  def renderDisplay: String = render

sealed private[ming] trait NumberValue extends Value

sealed private[ming] trait ProcedureValue extends Value

final private[ming] case class IntValue(value: BigInt) extends NumberValue:
  override def render: String = value.toString

final private[ming] case class RationalValue(numerator: BigInt, denominator: BigInt) extends NumberValue:
  override def render: String = s"$numerator/$denominator"

final private[ming] case class InexactValue(value: Double) extends NumberValue:
  override def render: String = java.lang.Double.toString(value)

final private[ming] case class BoolValue(value: Boolean) extends Value:
  override def render: String = if value then "#t" else "#f"

sealed private[ming] trait StringLikeValue extends Value:
  def text: String

  override def render: String =
    val escaped = text.flatMap:
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch   => ch.toString
    s"\"$escaped\""

  override def renderDisplay: String = text

final private[ming] case class StringValue(text: String) extends StringLikeValue

final private[ming] case class MutableStringValue(private var current: String) extends StringLikeValue:
  override def text: String = current

  def update(index: Int, value: Char): Unit =
    current = current.updated(index, value)

final private[ming] case class CharValue(value: Char) extends Value:

  override def render: String =
    value match
      case ' '  => "#\\space"
      case '\n' => "#\\newline"
      case ch   => s"#\\$ch"

  override def renderDisplay: String = value.toString

final private[ming] case class SymbolValue(name: String) extends Value:
  override def render: String = name

private[ming] case object EmptyListValue extends Value:
  override def render: String = "()"

final private[ming] class PairValue(private var currentCar: Value, private var currentCdr: Value) extends Value:
  def car: Value = currentCar

  def cdr: Value = currentCdr

  def updateCar(value: Value): Unit =
    currentCar = value

  def updateCdr(value: Value): Unit =
    currentCdr = value

  override def render: String = ValueRenderer.render(this)

private[ming] object PairValue:

  def apply(car: Value, cdr: Value): PairValue =
    new PairValue(car, cdr)

  def unapply(pair: PairValue): Some[(Value, Value)] =
    Some((pair.car, pair.cdr))

final private[ming] class VectorValue(initialElements: Iterable[Value]) extends Value:
  private val elements: Array[Value] = initialElements.iterator.toArray

  def length: Int =
    elements.length

  def element(index: Int): Value =
    elements(index)

  def update(index: Int, value: Value): Unit =
    elements(index) = value

  def toList: List[Value] =
    elements.toList

  override def render: String = ValueRenderer.render(this)

final private[ming] case class BuiltinValue(
  name: String,
  implementation: (List[Value], Position, Continuation) => EvaluationStep
) extends ProcedureValue:
  override def render: String = s"#<procedure:$name>"

private[ming] object BuiltinValue:

  def apply(
    name: String,
    implementation: (List[Value], Position) => Value
  ): BuiltinValue =
    new BuiltinValue(
      name,
      (arguments, position, continuation) =>
        InterpreterEvaluator.done(implementation(arguments, position), continuation)
    )

final private[ming] case class ClosureValue(
  parameters: List[String],
  restParameter: Option[String],
  body: List[Expr],
  env: Environment,
  name: Option[String] = None
) extends ProcedureValue:

  override def render: String =
    name match
      case Some(procedureName) => s"#<procedure:$procedureName>"
      case None                => "#<procedure:lambda>"

final private[ming] case class CaseLambdaValue(
  clauses: List[ClosureValue],
  name: Option[String] = None
) extends ProcedureValue:

  override def render: String =
    name match
      case Some(procedureName) => s"#<procedure:$procedureName>"
      case None                => "#<procedure:case-lambda>"

final private[ming] case class ContinuationValue(
  continuation: Continuation
) extends ProcedureValue:
  override def render: String = "#<procedure:continuation>"

final private[ming] class RecordTypeDescriptor(
  val name: String,
  val fieldNames: List[String]
)

final private[ming] class RecordValue(
  val recordType: RecordTypeDescriptor,
  initialFields: List[Value]
) extends Value:
  private val fields: Array[Value] = initialFields.toArray

  def field(index: Int): Value =
    fields(index)

  def updateField(index: Int, value: Value): Unit =
    fields(index) = value

  override def render: String = s"#<record:${recordType.name}>"

private[ming] case object VoidValue extends Value:
  override def render: String = "#<void>"

private[ming] case object UninitializedValue extends Value:
  override def render: String = "#<uninitialized>"

private[ming] object ValueRenderer:

  def render(value: Value): String =
    renderValue(value, mutable.HashSet.empty[AnyRef])

  private def renderValue(value: Value, active: mutable.HashSet[AnyRef]): String =
    value match
      case pair: PairValue     => renderPair(pair, active)
      case vector: VectorValue => renderVector(vector, active)
      case other               => other.render

  private def renderPair(pair: PairValue, active: mutable.HashSet[AnyRef]): String =
    val entered = mutable.ArrayBuffer.empty[AnyRef]

    def enter(ref: AnyRef): Boolean =
      if active.contains(ref) then false
      else
        active += ref
        entered += ref
        true

    if !enter(pair) then "#<cycle>"
    else
      try
        val builder        = StringBuilder("(")
        var current: Value = pair
        var first          = true
        var done           = false

        while !done do
          current match
            case currentPair: PairValue =>
              val nextValue = appendPairCell(currentPair, first, builder, enter)
              done = nextValue.isEmpty
              nextValue.foreach { next =>
                builder.append(renderValue(currentPair.car, active))
                current = next
                first = false
              }

            case EmptyListValue =>
              done = true

            case other =>
              builder.append(" . ")
              builder.append(renderValue(other, active))
              done = true

        builder.append(")")
        builder.result()
      finally entered.foreach(active.remove)

  private def appendPairCell(
    pair: PairValue,
    first: Boolean,
    builder: StringBuilder,
    enter: AnyRef => Boolean
  ): Option[Value] =
    if first then Some(pair.cdr)
    else if !enter(pair) then
      builder.append(" . #<cycle>")
      None
    else
      builder.append(" ")
      Some(pair.cdr)

  private def renderVector(vector: VectorValue, active: mutable.HashSet[AnyRef]): String =
    if active.contains(vector) then "#<cycle>"
    else
      active += vector
      try vector.toList.iterator.map(renderValue(_, active)).mkString("#(", " ", ")")
      finally active -= vector
