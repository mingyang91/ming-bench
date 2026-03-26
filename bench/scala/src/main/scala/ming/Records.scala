package ming

import scala.collection.mutable

object Records:

  def evalDefineRecordType(args: List[Expr], env: Env): SchemeVal =
    args match
      case Symbol(typeName, _) :: SList(Symbol(ctorName, _) :: ctorFields, _) :: Symbol(
            predName,
            _
          ) :: fieldDefs =>
        val ctorFieldNames = ctorFields.map {
          case Symbol(n, _) => n
          case _            => throw new EvalError("define-record-type: expected field name in constructor")
        }
        val fieldAccessors = fieldDefs.map {
          case SList(Symbol(fieldName, _) :: Symbol(accessorName, _) :: Nil, _) =>
            (fieldName, accessorName)
          case _ => throw new EvalError("define-record-type: bad field spec")
        }
        installConstructor(env, ctorName, typeName, ctorFieldNames)
        installPredicate(env, predName, typeName)
        for (fieldName, accessorName) <- fieldAccessors do installAccessor(env, accessorName, typeName, fieldName)
        SchemeVoid
      case _ => throw new EvalError("define-record-type: bad syntax")

  private def installConstructor(
    env: Env,
    ctorName: String,
    typeName: String,
    ctorFieldNames: List[String]
  ): Unit =
    env.set(
      ctorName,
      SchemeBuiltin(
        ctorName,
        { args =>
          if args.size != ctorFieldNames.size then
            throw new EvalError(s"$ctorName: expected ${ctorFieldNames.size} arguments, got ${args.size}")
          val fields = mutable.Map.from(ctorFieldNames.zip(args))
          SchemeRecord(typeName, fields)
        }
      )
    )

  private def installPredicate(env: Env, predName: String, typeName: String): Unit =
    env.set(
      predName,
      SchemeBuiltin(
        predName,
        {
          case rec :: Nil =>
            rec match
              case SchemeRecord(tn, _) => SchemeBool(tn == typeName)
              case _                   => SchemeBool(false)
          case args => throw new EvalError(s"$predName: expected 1 argument, got ${args.size}")
        }
      )
    )

  private def installAccessor(
    env: Env,
    accessorName: String,
    typeName: String,
    fieldName: String
  ): Unit =
    env.set(
      accessorName,
      SchemeBuiltin(
        accessorName,
        {
          case SchemeRecord(tn, fields) :: Nil =>
            if tn != typeName then throw new EvalError(s"$accessorName: expected $typeName, got $tn")
            fields.getOrElse(fieldName, throw new EvalError(s"$accessorName: field $fieldName not found"))
          case _ :: Nil => throw new EvalError(s"$accessorName: expected $typeName")
          case args     => throw new EvalError(s"$accessorName: expected 1 argument, got ${args.size}")
        }
      )
    )
