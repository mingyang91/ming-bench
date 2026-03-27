package ming

import BuiltinSupport.requireArgCount

private[ming] object SchemeRecords:

  def defineRecordType(args: List[Expr], env: Env, pos: SourcePos): Value =
    val recordType = parseDefinition(args, pos)

    env.define(recordType.constructorName, Value.RecordConstructor(recordType))
    env.define(recordType.predicateName, Value.RecordPredicate(recordType))
    recordType.fields.zipWithIndex.foreach { case (field, index) =>
      env.define(field.accessorName, Value.RecordAccessor(recordType, index, field.accessorName))
    }

    Value.Void

  def construct(recordType: RecordType, args: List[Value], pos: SourcePos): Value =
    requireArgCount(recordType.constructorName, args, recordType.constructorFieldIndices.length, pos)

    val storage = Array.fill[Value](recordType.fields.length)(Value.Void)
    recordType.constructorFieldIndices.zip(args).foreach { case (fieldIndex, value) =>
      storage(fieldIndex) = value
    }
    Value.RecordVal(new RecordInstance(recordType, storage))

  def test(recordType: RecordType, args: List[Value], pos: SourcePos): Value =
    val value = requireArgCount(recordType.predicateName, args, expected = 1, pos).head
    Value.BoolVal(isRecordOfType(recordType, value))

  def access(recordType: RecordType, fieldIndex: Int, name: String, args: List[Value], pos: SourcePos): Value =
    val record = requireArgCount(name, args, expected = 1, pos).head
    requireRecordOfType(recordType, record, name, pos).field(fieldIndex)

  private def parseDefinition(args: List[Expr], pos: SourcePos): RecordType =
    args match
      case Expr.Symbol(typeName, _) :: constructorExpr :: Expr.Symbol(predicateName, _) :: fieldExprs =>
        val (constructorName, constructorFields) = parseConstructor(constructorExpr, pos)
        val fields                               = fieldExprs.map(parseFieldSpec).toVector
        val fieldIndices                         = resolveFieldIndices(typeName, constructorFields, fields, pos)
        new RecordType(typeName, constructorName, predicateName, fieldIndices, fields)
      case _ =>
        throw EvalError.at(pos, "invalid define-record-type")

  private def parseConstructor(expr: Expr, pos: SourcePos): (String, Vector[String]) =
    expr match
      case Expr.ListExpr(Expr.Symbol(name, _) :: fieldExprs, _) =>
        val fieldNames = fieldExprs.map(parseSymbol(_, "record constructor field")).toVector
        (name, fieldNames)
      case _ =>
        throw EvalError.at(pos, "record constructor must be a list")

  private def parseFieldSpec(expr: Expr): RecordFieldSpec =
    expr match
      case Expr.ListExpr(
            Expr.Symbol(fieldName, _) :: Expr.Symbol(accessorName, _) :: Nil,
            _
          ) =>
        RecordFieldSpec(fieldName, accessorName)
      case _ =>
        throw EvalError.at(expr.pos, "record field spec must contain a field name and accessor name")

  private def parseSymbol(expr: Expr, context: String): String =
    expr match
      case Expr.Symbol(name, _) => name
      case _                    => throw EvalError.at(expr.pos, s"$context must be a symbol")

  private def resolveFieldIndices(
    typeName: String,
    constructorFields: Vector[String],
    fields: Vector[RecordFieldSpec],
    pos: SourcePos
  ): Vector[Int] =
    val fieldIndices = fields.iterator.zipWithIndex.map { case (field, index) =>
      field.fieldName -> index
    }.toMap

    constructorFields.map { fieldName =>
      fieldIndices.getOrElse(
        fieldName,
        throw EvalError.at(pos, s"$typeName constructor references unknown field: $fieldName")
      )
    }

  private def isRecordOfType(recordType: RecordType, value: Value): Boolean =
    value match
      case Value.RecordVal(instance) => instance.recordType eq recordType
      case _                         => false

  private def requireRecordOfType(recordType: RecordType, value: Value, name: String, pos: SourcePos): RecordInstance =
    value match
      case Value.RecordVal(instance) if instance.recordType eq recordType =>
        instance
      case other =>
        throw EvalError.at(pos, s"$name expected ${recordType.typeName}, got ${ValueSemantics.typeName(other)}")
