package ming

import SchemeTypes.{errAt, Env, Pos, Value}

object Records:

  def evalDefineRecordType(
    rest: List[Expr],
    env: Env,
    pos: Pos
  ): Value =
    rest match
      case Expr.Symbol(typeName, _) ::
          Expr.SList(Expr.Symbol(ctorName, _) :: ctorFields, _) ::
          Expr.Symbol(predName, _) ::
          fieldDefs =>
        val ctorFieldNames = ctorFields.map {
          case Expr.Symbol(n, _) => n
          case _                 => throw errAt(pos, "invalid define-record-type")
        }
        val fieldAccessors = fieldDefs.map {
          case Expr.SList(Expr.Symbol(fieldName, _) :: Expr.Symbol(accessorName, _) :: Nil, _) =>
            (fieldName, accessorName)
          case _ => throw errAt(pos, "invalid define-record-type field")
        }
        val recordTag = typeName

        env.define(ctorName, Value.VBuiltin(s"__record-ctor:$recordTag:${ctorFieldNames.mkString(",")}"))
        env.define(predName, Value.VBuiltin(s"__record-pred:$recordTag"))

        for (fieldName, accessorName) <- fieldAccessors do
          env.define(accessorName, Value.VBuiltin(s"__record-accessor:$recordTag:$fieldName"))

        Value.VVoid
      case _ => throw errAt(pos, "invalid define-record-type")

  def applyRecordOp(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    if name.startsWith("__record-ctor:") then
      val parts      = name.stripPrefix("__record-ctor:").split(":", 2)
      val typeName   = parts(0)
      val fieldNames = if parts(1).isEmpty then Nil else parts(1).split(",").toList
      if args.length != fieldNames.length then throw errAt(pos, s"$typeName constructor: wrong number of arguments")
      val fields = fieldNames.zip(args).toMap
      Value.VRecord(typeName, fields)
    else if name.startsWith("__record-pred:") then
      val typeName = name.stripPrefix("__record-pred:")
      if args.length != 1 then throw errAt(pos, "predicate requires 1 argument")
      args.head match
        case Value.VRecord(t, _) => Value.VBool(t == typeName)
        case _                   => Value.VBool(false)
    else if name.startsWith("__record-accessor:") then
      val parts     = name.stripPrefix("__record-accessor:").split(":", 2)
      val typeName  = parts(0)
      val fieldName = parts(1)
      if args.length != 1 then throw errAt(pos, "accessor requires 1 argument")
      args.head match
        case Value.VRecord(t, fields) if t == typeName =>
          fields.getOrElse(fieldName, throw errAt(pos, s"no field $fieldName"))
        case _ => throw errAt(pos, s"not a $typeName record")
    else throw errAt(pos, s"unknown record op: $name")
