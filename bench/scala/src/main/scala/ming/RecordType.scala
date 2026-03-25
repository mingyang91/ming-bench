package ming

/** Evaluation of define-record-type forms. */
object RecordType:

  def evalDefineRecordType(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(typeName) :: SchemeVal.SList(ctorElems) :: SchemeVal.Symbol(
            predName
          ) :: fieldDefs =>
        val ctorName = ctorElems.head match
          case SchemeVal.Symbol(n) => n
          case other               => throw new EvalError(s"define-record-type: expected constructor name, got $other")
        val ctorFields = ctorElems.tail.map {
          case SchemeVal.Symbol(f) => f
          case other               => throw new EvalError(s"define-record-type: expected field name, got $other")
        }

        val fieldAccessors = fieldDefs.map {
          case SchemeVal.SList(List(SchemeVal.Symbol(fieldName), SchemeVal.Symbol(accessorName))) =>
            (fieldName, accessorName)
          case other => throw new EvalError(s"define-record-type: bad field spec: $other")
        }

        defineConstructor(env, ctorName, ctorFields, typeName)
        definePredicate(env, predName, typeName)
        defineAccessors(env, fieldAccessors, typeName)

        SchemeVal.Void
      case _ => throw new EvalError("define-record-type: bad syntax")

  private def defineConstructor(
    env: Env,
    ctorName: String,
    ctorFields: List[String],
    typeName: String
  ): Unit =
    env.define(
      ctorName,
      SchemeVal.BuiltinProc(
        ctorName,
        { args =>
          if args.size != ctorFields.size then
            throw new EvalError(s"$ctorName: expected ${ctorFields.size} arguments, got ${args.size}")
          val fields = scala.collection.mutable.Map[String, SchemeVal]()
          ctorFields.zip(args).foreach((f, v) => fields(f) = v)
          SchemeVal.RecordVal(typeName, fields)
        }
      )
    )

  private def definePredicate(env: Env, predName: String, typeName: String): Unit =
    env.define(
      predName,
      SchemeVal.BuiltinProc(
        predName,
        {
          case List(SchemeVal.RecordVal(tn, _)) => SchemeVal.BoolVal(tn == typeName)
          case List(_)                          => SchemeVal.BoolVal(false)
          case args => throw new EvalError(s"$predName: expected 1 argument, got ${args.size}")
        }
      )
    )

  private def defineAccessors(
    env: Env,
    fieldAccessors: List[(String, String)],
    typeName: String
  ): Unit =
    for (fieldName, accessorName) <- fieldAccessors do
      env.define(
        accessorName,
        SchemeVal.BuiltinProc(
          accessorName,
          {
            case List(SchemeVal.RecordVal(tn, fields)) if tn == typeName =>
              fields.getOrElse(fieldName, throw new EvalError(s"$accessorName: no field $fieldName"))
            case List(other) =>
              throw new EvalError(s"$accessorName: expected $typeName, got ${SchemeVal.display(other)}")
            case args =>
              throw new EvalError(s"$accessorName: expected 1 argument, got ${args.size}")
          }
        )
      )
