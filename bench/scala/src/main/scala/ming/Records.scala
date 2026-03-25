package ming

final private[ming] class RecordType private (
  val name: String,
  val fieldNames: Vector[String],
  private val fieldIndexes: Map[String, Int]
):

  def fieldIndex(fieldName: String): Option[Int] =
    fieldIndexes.get(fieldName)

  def displayName: String =
    if name.length >= 2 && name.startsWith("<") && name.endsWith(">") then name.substring(1, name.length - 1)
    else name

private[ming] object RecordType:

  def apply(name: String, fieldNames: Vector[String]): RecordType =
    new RecordType(name, fieldNames, fieldNames.zipWithIndex.toMap)

private[ming] object RecordSupport extends BuiltinSupport:

  private case class RecordField(name: String, accessor: String, mutator: Option[String])

  private case class RecordDefinition(
    typeName: String,
    constructorName: String,
    constructorFields: Vector[String],
    predicateName: String,
    fields: Vector[RecordField]
  )

  def define(args: List[Expr], env: Env, pos: SourcePos): Value =
    val definition = parseDefinition(args, pos)
    validateDefinition(definition, pos)

    val recordType = RecordType(definition.typeName, definition.fields.map(_.name))

    env.define(
      definition.constructorName,
      buildConstructor(definition.constructorName, definition.constructorFields, recordType)
    )
    env.define(definition.predicateName, buildPredicate(definition.predicateName, recordType))

    definition.fields.foreach { field =>
      val fieldIndex = recordType.fieldIndex(field.name).get
      env.define(field.accessor, buildAccessor(field.accessor, recordType, fieldIndex))
      field.mutator.foreach(name => env.define(name, buildMutator(name, recordType, fieldIndex)))
    }

    Value.VoidVal

  private def parseDefinition(args: List[Expr], pos: SourcePos): RecordDefinition =
    args match
      case Expr.Symbol(typeName, _) ::
          Expr.ListExpr(Expr.Symbol(constructorName, _) :: constructorFields, constructorPos) ::
          Expr.Symbol(predicateName, _) ::
          fieldSpecs if fieldSpecs.nonEmpty =>
        RecordDefinition(
          typeName,
          constructorName,
          constructorFields.map(asSymbol(_, constructorPos)).toVector,
          predicateName,
          fieldSpecs.map(parseField).toVector
        )

      case _ =>
        throw EvalError.at(
          pos,
          "define-record-type expects a type name, constructor, predicate, and at least 1 field"
        )

  private def parseField(fieldExpr: Expr): RecordField =
    fieldExpr match
      case Expr.ListExpr(Expr.Symbol(fieldName, _) :: Expr.Symbol(accessorName, _) :: Nil, _) =>
        RecordField(fieldName, accessorName, None)

      case Expr.ListExpr(
            Expr.Symbol(fieldName, _) :: Expr.Symbol(accessorName, _) :: Expr.Symbol(mutatorName, _) :: Nil,
            _
          ) =>
        RecordField(fieldName, accessorName, Some(mutatorName))

      case Expr.ListExpr(_, fieldPos) =>
        throw EvalError.at(fieldPos, "record field spec must contain a field name and accessor")

      case other =>
        throw EvalError.at(MacroSupport.exprPos(other), "record field spec must be a list")

  private def validateDefinition(definition: RecordDefinition, pos: SourcePos): Unit =
    val fieldNames = definition.fields.map(_.name)

    ensureDistinct(fieldNames, pos, "record field")
    ensureDistinct(definition.constructorFields, pos, "constructor field")
    ensureDistinct(definition.fields.map(_.accessor), pos, "record accessor")
    ensureDistinct(definition.fields.flatMap(_.mutator), pos, "record mutator")

    if definition.constructorFields.length != fieldNames.length then
      throw EvalError.at(pos, "constructor must initialize every record field exactly once")

    definition.constructorFields.foreach { fieldName =>
      if !fieldNames.contains(fieldName) then
        throw EvalError.at(pos, s"constructor references unknown record field: $fieldName")
    }

  private def ensureDistinct(names: Seq[String], pos: SourcePos, description: String): Unit =
    names.groupBy(identity).collectFirst { case (name, copies) if copies.sizeCompare(1) > 0 => name }.foreach { name =>
      throw EvalError.at(pos, s"duplicate $description: $name")
    }

  private def asSymbol(expr: Expr, pos: SourcePos): String =
    expr match
      case Expr.Symbol(name, _) =>
        name

      case _ =>
        throw EvalError.at(pos, "record constructor fields must be symbols")

  private def buildConstructor(name: String, constructorFields: Vector[String], recordType: RecordType): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        if args.length != constructorFields.length then
          throw EvalError.at(pos, s"$name expects exactly ${constructorFields.length} arguments")

        val fields = Array.fill[Value](recordType.fieldNames.length)(Value.VoidVal)
        constructorFields.zip(args).foreach { case (fieldName, value) =>
          fields(recordType.fieldIndex(fieldName).get) = value
        }
        Value.RecordVal(recordType, fields)
    )

  private def buildPredicate(name: String, recordType: RecordType): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        Value.BoolVal(
          expectSingleArg(args, pos, name) match
            case Value.RecordVal(actualType, _) =>
              actualType eq recordType

            case _ =>
              false
        )
    )

  private def buildAccessor(name: String, recordType: RecordType, fieldIndex: Int): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        expectSingleArg(args, pos, name) match
          case Value.RecordVal(actualType, fields) if actualType eq recordType =>
            fields(fieldIndex)

          case Value.RecordVal(_, _) =>
            throw EvalError.at(pos, s"$name expected a ${recordType.name} record")

          case other =>
            throw EvalError.at(pos, s"$name expected a ${recordType.name} record, got ${other.typeName}")
    )

  private def buildMutator(name: String, recordType: RecordType, fieldIndex: Int): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val (record, replacement) = expectTwoArgs(args, pos, name)
        record match
          case Value.RecordVal(actualType, fields) if actualType eq recordType =>
            fields(fieldIndex) = replacement
            Value.VoidVal

          case Value.RecordVal(_, _) =>
            throw EvalError.at(pos, s"$name expected a ${recordType.name} record")

          case other =>
            throw EvalError.at(pos, s"$name expected a ${recordType.name} record, got ${other.typeName}")
    )
