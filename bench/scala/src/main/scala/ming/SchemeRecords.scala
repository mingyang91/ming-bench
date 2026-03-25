package ming

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

private[ming] object SchemeRecords:
  import SchemeInterpreter.{Expr, Value}

  final private[ming] case class RecordTypeDescriptor(
    id: Long,
    name: String,
    fieldNames: Vector[String]
  )

  final private case class RecordFieldSpec(
    fieldName: String,
    accessorName: String,
    mutatorName: Option[String]
  )

  final private case class RecordDefinition(
    typeName: String,
    constructorName: String,
    constructorPos: SourcePos,
    constructorFieldNames: List[String],
    predicateName: String,
    fieldSpecs: List[RecordFieldSpec]
  ):

    def declaredFieldNames: List[String] =
      fieldSpecs.map(_.fieldName)

  private val nextRecordTypeId = new AtomicLong(0L)

  def evalDefineRecordType(args: List[Expr], env: Env, pos: SourcePos): Value =
    val definition = parseRecordDefinition(args, pos)
    validateConstructorFields(definition, pos)

    val recordType = RecordTypeDescriptor(
      id = nextRecordTypeId.incrementAndGet(),
      name = definition.typeName,
      fieldNames = definition.declaredFieldNames.toVector
    )

    env.define(definition.constructorName, recordConstructor(definition.constructorName, recordType))
    env.define(definition.predicateName, recordPredicate(definition.predicateName, recordType))
    registerFieldBindings(definition.fieldSpecs, recordType, env)
    Value.Void

  private def parseRecordDefinition(args: List[Expr], pos: SourcePos): RecordDefinition =
    args match
      case Expr.Symbol(typeName, _) ::
          Expr.ListExpr(Expr.Symbol(constructorName, _) :: constructorFields, constructorPos) ::
          Expr.Symbol(predicateName, _) ::
          fieldSpecs =>
        RecordDefinition(
          typeName = typeName,
          constructorName = constructorName,
          constructorPos = constructorPos,
          constructorFieldNames = constructorFields.map(readRecordFieldName),
          predicateName = predicateName,
          fieldSpecs = fieldSpecs.map(readRecordFieldSpec)
        )
      case _ =>
        throw EvalError.at(pos, "invalid define-record-type")

  private def validateConstructorFields(definition: RecordDefinition, pos: SourcePos): Unit =
    val declaredFieldNames = definition.declaredFieldNames

    if definition.constructorFieldNames.length != declaredFieldNames.length then
      throw EvalError.at(
        definition.constructorPos,
        s"define-record-type constructor expected ${declaredFieldNames.length} fields, got ${definition.constructorFieldNames.length}"
      )

    if definition.constructorFieldNames != declaredFieldNames then
      throw EvalError.at(pos, "define-record-type constructor fields must match field declarations")

  private def registerFieldBindings(
    fieldSpecs: List[RecordFieldSpec],
    recordType: RecordTypeDescriptor,
    env: Env
  ): Unit =
    fieldSpecs.zipWithIndex.foreach { case (fieldSpec, index) =>
      env.define(
        fieldSpec.accessorName,
        recordAccessor(fieldSpec.accessorName, recordType, index)
      )
      fieldSpec.mutatorName.foreach { mutatorName =>
        env.define(mutatorName, recordMutator(mutatorName, recordType, index))
      }
    }

  private def recordConstructor(
    name: String,
    recordType: RecordTypeDescriptor
  ): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        BuiltinSupport.requireExactly(name, args, recordType.fieldNames.length, pos)
        new Value.Record(recordType, mutable.ArrayBuffer.from(args))
    )

  private def recordPredicate(
    name: String,
    recordType: RecordTypeDescriptor
  ): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        Value.Bool(
          BuiltinSupport.singleArg(name, args, pos) match
            case record: Value.Record => record.recordTypeId == recordType.id
            case _                    => false
        )
    )

  private def recordAccessor(
    name: String,
    recordType: RecordTypeDescriptor,
    index: Int
  ): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        val record = expectRecord(BuiltinSupport.singleArg(name, args, pos), recordType, name, pos)
        record.field(index)
    )

  private def recordMutator(
    name: String,
    recordType: RecordTypeDescriptor,
    index: Int
  ): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) =>
        BuiltinSupport.requireExactly(name, args, expected = 2, pos)
        args match
          case recordValue :: newValue :: Nil =>
            val record = expectRecord(recordValue, recordType, name, pos)
            record.setField(index, newValue)
            Value.Void
          case _ =>
            BuiltinSupport.unreachable()
    )

  private def expectRecord(
    value: Value,
    recordType: RecordTypeDescriptor,
    context: String,
    pos: SourcePos
  ): Value.Record =
    value match
      case record: Value.Record if record.recordTypeId == recordType.id =>
        record
      case other =>
        throw EvalError.at(pos, s"$context expected ${recordType.name}, got ${SchemeInterpreter.render(other)}")

  private def readRecordFieldName(expr: Expr): String =
    expr match
      case Expr.Symbol(name, _) if name != "." =>
        name
      case other =>
        throw EvalError.at(other.pos, s"invalid record field: ${SchemeRendering.renderExpr(other)}")

  private def readRecordFieldSpec(expr: Expr): RecordFieldSpec =
    expr match
      case Expr.ListExpr(
            List(Expr.Symbol(fieldName, _), Expr.Symbol(accessorName, _)),
            _
          ) =>
        RecordFieldSpec(fieldName, accessorName, None)
      case Expr.ListExpr(
            List(
              Expr.Symbol(fieldName, _),
              Expr.Symbol(accessorName, _),
              Expr.Symbol(mutatorName, _)
            ),
            _
          ) =>
        RecordFieldSpec(fieldName, accessorName, Some(mutatorName))
      case other =>
        throw EvalError.at(
          other.pos,
          s"invalid record field specification: ${SchemeRendering.renderExpr(other)}"
        )
