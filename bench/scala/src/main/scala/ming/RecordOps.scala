package ming

/** Record type definition and operations. */
object RecordOps:

  def evalDefineRecordType(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(typeName) ::
          SchemeVal.SList(SchemeVal.SSymbol(ctorName) :: ctorFields) ::
          SchemeVal.SSymbol(predName) ::
          fieldDefs =>
        val ctorFieldNames = ctorFields.map {
          case SchemeVal.SSymbol(n) => n
          case other => throw new EvalError(s"define-record-type: expected field name, got ${other.display}")
        }
        val fieldAccessors = fieldDefs.map {
          case SchemeVal.SList(SchemeVal.SSymbol(fieldName) :: SchemeVal.SSymbol(accessorName) :: Nil) =>
            (fieldName, accessorName)
          case other => throw new EvalError(s"define-record-type: bad field spec ${other.display}")
        }
        env.define(ctorName, SchemeVal.SSymbol(s"__record-ctor__:$typeName:${ctorFieldNames.mkString(",")}"))
        env.define(predName, SchemeVal.SSymbol(s"__record-pred__:$typeName"))
        fieldAccessors.foreach { (fieldName, accessorName) =>
          env.define(accessorName, SchemeVal.SSymbol(s"__record-acc__:$typeName:$fieldName"))
        }
        SchemeVal.SVoid
      case _ => throw new EvalError("define-record-type: bad syntax")

  def applyRecordOp(name: String, args: List[SchemeVal]): SchemeVal =
    if name.startsWith("__record-ctor__:") then applyRecordCtor(name, args)
    else if name.startsWith("__record-pred__:") then applyRecordPred(name, args)
    else applyRecordAccessor(name, args)

  private def applyRecordCtor(name: String, args: List[SchemeVal]): SchemeVal =
    val parts      = name.stripPrefix("__record-ctor__:").split(":", 2)
    val typeName   = parts(0)
    val fieldNames = if parts(1).isEmpty then Nil else parts(1).split(",").toList
    if args.length != fieldNames.length then
      throw new EvalError(s"$typeName constructor: expected ${fieldNames.length} arguments, got ${args.length}")
    SchemeVal.SRecord(typeName, fieldNames.zip(args).toMap)

  private def applyRecordPred(name: String, args: List[SchemeVal]): SchemeVal =
    val typeName = name.stripPrefix("__record-pred__:")
    if args.length != 1 then throw new EvalError(s"$typeName?: expected 1 argument")
    args.head match
      case SchemeVal.SRecord(tn, _) => SchemeVal.SBool(tn == typeName)
      case _                        => SchemeVal.SBool(false)

  private def applyRecordAccessor(name: String, args: List[SchemeVal]): SchemeVal =
    val parts     = name.stripPrefix("__record-acc__:").split(":", 2)
    val typeName  = parts(0)
    val fieldName = parts(1)
    if args.length != 1 then throw new EvalError(s"accessor: expected 1 argument")
    args.head match
      case SchemeVal.SRecord(tn, fields) if tn == typeName =>
        fields.getOrElse(fieldName, throw new EvalError(s"record $typeName has no field $fieldName"))
      case other => throw new EvalError(s"$typeName accessor: expected $typeName record, got ${other.display}")
