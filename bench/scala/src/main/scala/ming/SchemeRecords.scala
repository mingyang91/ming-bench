package ming

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeRecords:

  final private case class ConstructorSpec(name: String, fieldNames: Vector[String])

  final private case class FieldSpec(name: String, accessorName: String)

  def defineRecordType(args: List[Expr], env: Env): Value =
    val (typeName, constructor, predicateName, fields) = parseDefinition(args)
    val recordType                                     = new RecordType(typeName, fields.map(_.name))

    env.define(constructor.name, constructorBuiltin(constructor.name, recordType))
    env.define(predicateName, predicateBuiltin(predicateName, recordType))
    fields.zipWithIndex.foreach { case (field, index) =>
      env.define(field.accessorName, accessorBuiltin(field.accessorName, recordType, index))
    }

    Value.VoidValue

  private def parseDefinition(args: List[Expr]): (String, ConstructorSpec, String, Vector[FieldSpec]) =
    args match
      case Expr.Symbol(typeName, _) :: constructorExpr :: Expr.Symbol(predicateName, _) :: fieldExprs =>
        val constructor = parseConstructor(constructorExpr)
        val fields      = fieldExprs.map(parseField).toVector

        ensureDistinct(constructor.fieldNames.toList, "record constructor fields")
        ensureDistinct(fields.map(_.name).toList, "record fields")
        ensureDistinct(fields.map(_.accessorName).toList, "record accessors")

        if constructor.fieldNames != fields.map(_.name) then
          throw new EvalError("record constructor fields must match field specifications")

        (typeName, constructor, predicateName, fields)
      case _ =>
        throw new EvalError("invalid define-record-type form")

  private def parseConstructor(expr: Expr): ConstructorSpec =
    expr match
      case Expr.ListExpr(Expr.Symbol(name, _) :: fieldExprs, _) =>
        ConstructorSpec(name, fieldExprs.map(requireFieldName).toVector)
      case _ =>
        throw new EvalError("record constructor must be a list")

  private def parseField(expr: Expr): FieldSpec =
    expr match
      case Expr.ListExpr(List(Expr.Symbol(name, _), Expr.Symbol(accessorName, _)), _) =>
        FieldSpec(name, accessorName)
      case _ =>
        throw new EvalError("record field specifications must contain a field name and accessor name")

  private def requireFieldName(expr: Expr): String =
    expr match
      case Expr.Symbol(name, _) => name
      case _                    => throw new EvalError("record field names must be symbols")

  private def constructorBuiltin(name: String, recordType: RecordType): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, recordType.fieldCount)
        Value.RecordValue(recordType, args.toVector)
    )

  private def predicateBuiltin(name: String, recordType: RecordType): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(isRecordOfType(args.head, recordType))
    )

  private def accessorBuiltin(name: String, recordType: RecordType, index: Int): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        args.head match
          case Value.RecordValue(valueType, fields) if valueType.eq(recordType) =>
            fields(index)
          case other =>
            throw new EvalError(
              s"$name expected a ${recordType.name} record, got ${SchemeRuntime.render(other)}"
            )
    )

  private def isRecordOfType(value: Value, recordType: RecordType): Boolean =
    value match
      case Value.RecordValue(valueType, _) => valueType.eq(recordType)
      case _                               => false
