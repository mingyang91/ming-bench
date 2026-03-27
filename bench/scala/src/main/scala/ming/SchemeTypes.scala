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

private[ming] enum Value:
  case IntVal(value: Long)
  case RationalVal(numerator: Long, denominator: Long)
  case InexactVal(value: Double)
  case BoolVal(value: Boolean)
  case StringVal(value: MutableString)
  case CharVal(value: Char)
  case SymbolVal(name: String)
  case EmptyList
  case PairVal(car: Value, cdr: Value)
  case VectorVal(instance: VectorInstance)
  case BuiltinProc(name: String)
  case RecordConstructor(recordType: RecordType)
  case RecordPredicate(recordType: RecordType)
  case RecordAccessor(recordType: RecordType, fieldIndex: Int, name: String)
  case RecordVal(instance: RecordInstance)
  case CaseClosure(name: Option[String], clauses: List[CaseLambdaClause], env: Env)

  case Closure(
    name: Option[String],
    params: List[String],
    restParam: Option[String],
    body: List[Expr],
    env: Env
  )
  case Void

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
