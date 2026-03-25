package ming

private[ming] object RecordOps:

  private case class RecordTypeInfo(
    name: String,
    typeId: Int,
    fieldNames: Array[String],
    ctorFieldNames: List[String],
    ctorFieldIndices: List[Int]
  )

  private val recordTypeCounter                                          = new java.util.concurrent.atomic.AtomicInteger(0)
  private val recordTypes: java.util.concurrent.ConcurrentHashMap[Int, RecordTypeInfo] =
    new java.util.concurrent.ConcurrentHashMap[Int, RecordTypeInfo]()

  def evalDefineRecordType(args: List[Expr], env: Env): Expr =
    args match
      case Expr.Sym(typeName) :: Expr.Lst(Expr.Sym(ctorName) :: ctorFields) :: Expr.Sym(
            predName
          ) :: fieldSpecs =>
        val typeId = recordTypeCounter.incrementAndGet()
        val ctorFieldNames = ctorFields.map {
          case Expr.Sym(f) => f
          case _           => throw EvalError("define-record-type: invalid constructor field")
        }
        val allFieldNames = fieldSpecs.map {
          case Expr.Lst(List(Expr.Sym(fname), Expr.Sym(_))) => fname
          case _                                            => throw EvalError("define-record-type: invalid field spec")
        }
        val accessorNames = fieldSpecs.map {
          case Expr.Lst(List(Expr.Sym(_), Expr.Sym(acc))) => acc
          case _                                          => throw EvalError("define-record-type: invalid field spec")
        }
        val fieldIndexMap = allFieldNames.zipWithIndex.toMap

        val ctorIndices =
          ctorFieldNames.map(f => fieldIndexMap.getOrElse(f, throw EvalError(s"define-record-type: unknown field $f")))
        env.define(ctorName, Expr.Sym(s"%%record-ctor-$typeId"))
        env.define(predName, Expr.Sym(s"%%record-pred-$typeId"))
        accessorNames.zipWithIndex.foreach { (accName, idx) =>
          env.define(accName, Expr.Sym(s"%%record-acc-$typeId-$idx"))
        }
        recordTypes.put(typeId, RecordTypeInfo(typeName, typeId, allFieldNames.toArray, ctorFieldNames, ctorIndices))
        Expr.Bool(false)
      case _ => throw EvalError("define-record-type: invalid syntax")

  def applyRecordOp(name: String, args: List[Expr]): Expr =
    if name.startsWith("%%record-ctor-") then
      val typeId = name.stripPrefix("%%record-ctor-").toInt
      val info   = recordTypes.get(typeId)
      if args.length != info.ctorFieldNames.length then
        throw EvalError(
          s"${info.name} constructor: expected ${info.ctorFieldNames.length} arguments, got ${args.length}"
        )
      val fields = new Array[Expr](info.fieldNames.length)
      info.ctorFieldIndices.zip(args).foreach { (idx, v) => fields(idx) = v }
      Expr.Record(info.name, typeId, fields, info.fieldNames)
    else if name.startsWith("%%record-pred-") then
      val typeId = name.stripPrefix("%%record-pred-").toInt
      if args.length != 1 then throw EvalError("record predicate: expected 1 argument")
      args.head match
        case Expr.Record(_, tid, _, _) => Expr.Bool(tid == typeId)
        case _                         => Expr.Bool(false)
    else if name.startsWith("%%record-acc-") then
      val parts    = name.stripPrefix("%%record-acc-").split("-")
      val typeId   = parts(0).toInt
      val fieldIdx = parts(1).toInt
      if args.length != 1 then throw EvalError("record accessor: expected 1 argument")
      args.head match
        case Expr.Record(_, tid, fields, _) if tid == typeId => fields(fieldIdx)
        case _ => throw EvalError(s"record accessor: not a ${recordTypes.get(typeId).name}")
    else throw EvalError(s"unknown record operation: $name")
