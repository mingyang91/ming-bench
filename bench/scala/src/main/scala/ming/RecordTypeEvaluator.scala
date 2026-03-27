package ming

private[ming] object RecordTypeEvaluator:

  final private case class RecordTypeSpec(
    typeName: String,
    constructorName: String,
    constructorFields: List[String],
    predicateName: String,
    fields: List[RecordFieldSpec]
  )

  final private case class RecordFieldSpec(
    fieldName: String,
    accessorName: String,
    mutatorName: Option[String]
  )

  def evalDefineRecordType(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val spec       = parseRecordType(arguments, position)
    val recordType = new RecordTypeDescriptor(spec.typeName, spec.fields.map(_.fieldName))
    defineConstructor(spec, recordType, env)
    definePredicate(spec, recordType, env)
    defineFieldProcedures(spec.fields, recordType, env)
    VoidValue

  private def defineConstructor(
    spec: RecordTypeSpec,
    recordType: RecordTypeDescriptor,
    env: Environment
  ): Unit =
    env.define(
      spec.constructorName,
      BuiltinValue(
        spec.constructorName,
        recordConstructor(spec.constructorName, spec.constructorFields.length, recordType)
      )
    )

  private def definePredicate(
    spec: RecordTypeSpec,
    recordType: RecordTypeDescriptor,
    env: Environment
  ): Unit =
    env.define(
      spec.predicateName,
      BuiltinValue(spec.predicateName, recordPredicate(spec.predicateName, recordType))
    )

  private def defineFieldProcedures(
    fields: List[RecordFieldSpec],
    recordType: RecordTypeDescriptor,
    env: Environment
  ): Unit =
    fields.zipWithIndex.foreach { case (field, index) =>
      env.define(
        field.accessorName,
        BuiltinValue(field.accessorName, recordAccessor(field.accessorName, recordType, index))
      )
      field.mutatorName.foreach { mutatorName =>
        env.define(
          mutatorName,
          BuiltinValue(mutatorName, recordMutator(mutatorName, recordType, index))
        )
      }
    }

  private def parseRecordType(arguments: List[Expr], position: Position): RecordTypeSpec =
    arguments match
      case typeExpression :: constructorExpression :: predicateExpression :: fieldExpressions =>
        val (constructorName, constructorFields) = parseRecordConstructor(constructorExpression)
        val fields                               = fieldExpressions.map(parseRecordField)
        if constructorFields.length != fields.length then
          SchemeFailure.raise(
            "define-record-type expected one field clause per constructor field",
            position
          )
        RecordTypeSpec(
          expectIdentifier(typeExpression, "define-record-type expected a record type name"),
          constructorName,
          constructorFields,
          expectIdentifier(predicateExpression, "define-record-type expected a predicate name"),
          fields
        )
      case _ =>
        SchemeFailure.raise(
          "define-record-type expected a type name, constructor, predicate, and field clauses",
          position
        )

  private def parseRecordConstructor(expression: Expr): (String, List[String]) =
    expression match
      case ListExpr(SymbolExpr(constructorName, _) :: fieldExpressions, _) =>
        (
          constructorName,
          fieldExpressions.map(argument =>
            expectIdentifier(argument, "define-record-type constructor expected identifiers")
          )
        )
      case _ =>
        SchemeFailure.raise(
          "define-record-type expected a constructor clause of the form (name field ...)",
          expression.position
        )

  private def parseRecordField(expression: Expr): RecordFieldSpec =
    expression match
      case ListExpr(List(SymbolExpr(fieldName, _), SymbolExpr(accessorName, _)), _) =>
        RecordFieldSpec(fieldName, accessorName, None)
      case ListExpr(
            List(SymbolExpr(fieldName, _), SymbolExpr(accessorName, _), SymbolExpr(mutatorName, _)),
            _
          ) =>
        RecordFieldSpec(fieldName, accessorName, Some(mutatorName))
      case _ =>
        SchemeFailure.raise(
          "define-record-type expected field clauses of the form (field accessor) or (field accessor mutator)",
          expression.position
        )

  private def recordConstructor(
    constructorName: String,
    fieldCount: Int,
    recordType: RecordTypeDescriptor
  ): (List[Value], Position) => Value =
    (arguments, position) =>
      new RecordValue(
        recordType,
        RuntimeSupport.expectExact(arguments, fieldCount, constructorName, position)
      )

  private def recordPredicate(
    predicateName: String,
    recordType: RecordTypeDescriptor
  ): (List[Value], Position) => Value =
    (arguments, position) =>
      val value = RuntimeSupport.expectSingleArgument(arguments, predicateName, position)
      BoolValue(
        value match
          case record: RecordValue => record.recordType eq recordType
          case _                   => false
      )

  private def recordAccessor(
    accessorName: String,
    recordType: RecordTypeDescriptor,
    index: Int
  ): (List[Value], Position) => Value =
    (arguments, position) => expectRecord(arguments, accessorName, recordType, position).field(index)

  private def recordMutator(
    mutatorName: String,
    recordType: RecordTypeDescriptor,
    index: Int
  ): (List[Value], Position) => Value =
    (arguments, position) =>
      RuntimeSupport.expectExact(arguments, 2, mutatorName, position) match
        case recordValue :: newValue :: Nil =>
          expectRecordValue(recordValue, mutatorName, recordType, position).updateField(index, newValue)
          VoidValue
        case _ =>
          throw new IllegalStateException("validated two-argument record mutator call")

  private def expectRecord(
    arguments: List[Value],
    name: String,
    recordType: RecordTypeDescriptor,
    position: Position
  ): RecordValue =
    val value = RuntimeSupport.expectSingleArgument(arguments, name, position)
    expectRecordValue(value, name, recordType, position)

  private def expectRecordValue(
    value: Value,
    name: String,
    recordType: RecordTypeDescriptor,
    position: Position
  ): RecordValue =
    value match
      case record: RecordValue if record.recordType eq recordType =>
        record
      case _: RecordValue =>
        SchemeFailure.raise(s"$name expected a ${recordType.name} record", position)
      case other =>
        SchemeFailure.raise(
          s"$name expected a ${recordType.name} record, got ${RuntimeSupport.typeName(other)}",
          position
        )

  private def expectIdentifier(expression: Expr, message: String): String =
    expression match
      case SymbolExpr(name, _) => name
      case _                   => SchemeFailure.raise(message, expression.position)
