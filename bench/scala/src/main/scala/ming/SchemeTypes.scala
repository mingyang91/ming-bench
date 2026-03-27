package ming

private[ming] enum Expr:
  def pos: SourcePos

  case IntLit(value: Long, pos: SourcePos)                             extends Expr
  case RationalLit(numerator: Long, denominator: Long, pos: SourcePos) extends Expr
  case InexactLit(value: Double, pos: SourcePos)                       extends Expr
  case BoolLit(value: Boolean, pos: SourcePos)                         extends Expr
  case StringLit(value: String, pos: SourcePos)                        extends Expr
  case CharLit(value: Char, pos: SourcePos)                            extends Expr
  case Symbol(name: String, pos: SourcePos)                            extends Expr
  case ListExpr(items: List[Expr], pos: SourcePos)                     extends Expr

sealed private[ming] trait Value

private[ming] object Value:
  final case class IntVal(value: Long)                             extends Value
  final case class RationalVal(numerator: Long, denominator: Long) extends Value
  final case class InexactVal(value: Double)                       extends Value
  final case class BoolVal(value: Boolean)                         extends Value
  final case class StringVal(value: MutableString)                 extends Value
  final case class CharVal(value: Char)                            extends Value
  final case class SymbolVal(name: String)                         extends Value
  case object EmptyList                                            extends Value

  final class PairVal private (
    private var currentCar: Value,
    private var currentCdr: Value
  ) extends Value:

    def car: Value =
      currentCar

    def cdr: Value =
      currentCdr

    def setCar(value: Value): Unit =
      currentCar = value

    def setCdr(value: Value): Unit =
      currentCdr = value

  object PairVal:

    def apply(car: Value, cdr: Value): PairVal =
      new PairVal(car, cdr)

    def unapply(value: Value): Option[(Value, Value)] =
      value match
        case pair: PairVal => Some((pair.car, pair.cdr))
        case _             => None

  final case class VectorVal(instance: VectorInstance)                                          extends Value
  final case class BuiltinProc(name: String)                                                    extends Value
  final case class RecordConstructor(recordType: RecordType)                                    extends Value
  final case class RecordPredicate(recordType: RecordType)                                      extends Value
  final case class RecordAccessor(recordType: RecordType, fieldIndex: Int, name: String)        extends Value
  final case class RecordVal(instance: RecordInstance)                                          extends Value
  final case class CaseClosure(name: Option[String], clauses: List[CaseLambdaClause], env: Env) extends Value

  final case class Closure(
    name: Option[String],
    params: List[String],
    restParam: Option[String],
    body: List[Expr],
    env: Env
  ) extends Value

  case object Void extends Value

final private[ming] class MutableString private (
  private val builder: java.lang.StringBuilder,
  val isMutable: Boolean
):

  def length: Int =
    builder.length()

  def charAt(index: Int): Char =
    builder.charAt(index)

  def setCharAt(index: Int, ch: Char): Unit =
    builder.setCharAt(index, ch)

  def text: String =
    builder.toString

  override def toString: String =
    text

object MutableString:

  def from(text: String): MutableString =
    new MutableString(new java.lang.StringBuilder(text), isMutable = true)

  def immutable(text: String): MutableString =
    new MutableString(new java.lang.StringBuilder(text), isMutable = false)

final private[ming] class VectorInstance(
  private val storage: Array[Value]
):

  def length: Int =
    storage.length

  def elementAt(index: Int): Value =
    storage(index)

  def setElementAt(index: Int, value: Value): Unit =
    storage(index) = value

  def elements: List[Value] =
    storage.toList

final private[ming] case class RecordFieldSpec(
  fieldName: String,
  accessorName: String
)

final private[ming] case class CaseLambdaClause(
  params: List[String],
  restParam: Option[String],
  body: List[Expr]
):

  def matchesArgCount(actualArgCount: Int): Boolean =
    restParam match
      case Some(_) => actualArgCount >= params.length
      case None    => actualArgCount == params.length

final private[ming] class RecordType(
  val typeName: String,
  val constructorName: String,
  val predicateName: String,
  val constructorFieldIndices: Vector[Int],
  val fields: Vector[RecordFieldSpec]
)

final private[ming] class RecordInstance(
  val recordType: RecordType,
  private val storage: Array[Value]
):

  def field(index: Int): Value =
    storage(index)
