package ming

object RecordType:

  def defineRecordType(
    typeName: String,
    ctorName: String,
    ctorFields: List[Expr],
    predName: String,
    fieldDefs: List[Expr],
    env: Env
  ): SchemeVal =
    val fieldNames = ctorFields.map {
      case Expr.Symbol(n) => n
      case _              => throw new EvalError("define-record-type: expected field name")
    }
    env.define(
      ctorName,
      SchemeVal.BuiltinProc(
        ctorName,
        args =>
          if args.length != fieldNames.length then
            throw new EvalError(s"$ctorName: expected ${fieldNames.length} arguments, got ${args.length}")
          SchemeVal.RecordVal(typeName, fieldNames.zip(args).toMap)
      )
    )
    env.define(
      predName,
      SchemeVal.BuiltinProc(
        predName,
        {
          case SchemeVal.RecordVal(t, _) :: Nil => SchemeVal.BoolVal(t == typeName)
          case _ :: Nil                         => SchemeVal.BoolVal(false)
          case args => throw new EvalError(s"$predName: expected 1 argument, got ${args.length}")
        }
      )
    )
    for fd <- fieldDefs do
      fd match
        case Expr.SList(Expr.Symbol(fname) :: Expr.Symbol(accName) :: Nil) =>
          env.define(
            accName,
            SchemeVal.BuiltinProc(
              accName,
              {
                case SchemeVal.RecordVal(t, fields) :: Nil =>
                  if t != typeName then throw new EvalError(s"$accName: expected $typeName, got $t")
                  fields.getOrElse(fname, throw new EvalError(s"$accName: no field $fname"))
                case v :: Nil => throw new EvalError(s"$accName: expected $typeName, got ${v.display}")
                case args     => throw new EvalError(s"$accName: expected 1 argument, got ${args.length}")
              }
            )
          )
        case _ => throw new EvalError("define-record-type: invalid field spec")
    SchemeVal.Void
