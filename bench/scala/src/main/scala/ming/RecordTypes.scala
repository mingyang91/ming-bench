package ming

import SchemeValue.*
import Evaluator.{ReturnS, Step}

/** define-record-type implementation. */
private[ming] object RecordTypes:

  def evalDefineRecordType(
    args: List[SchemeValue],
    env: Env,
    k: Kont,
    out: String
  ): Step =
    args match
      case SchemeSymbol(typeName) :: SchemeList(
            SchemeSymbol(ctorName) :: ctorFields
          ) :: SchemeSymbol(predName) :: fieldSpecs =>
        val ctorFieldNames = ctorFields.map {
          case SchemeSymbol(n) => n
          case other           => throw new EvalError(s"define-record-type: bad field name: ${other.display}")
        }
        val accessors = fieldSpecs.map {
          case SchemeList(SchemeSymbol(fieldName) :: SchemeSymbol(accessorName) :: Nil) =>
            val idx = ctorFieldNames.indexOf(fieldName)
            if idx < 0 then throw new EvalError(s"define-record-type: unknown field $fieldName")
            (accessorName, idx)
          case other =>
            throw new EvalError(s"define-record-type: bad field spec: ${other.display}")
        }
        val tag = new AnyRef
        val ctor = SchemeNativeProc(
          ctorName,
          vals =>
            if vals.length != ctorFieldNames.length then
              throw new EvalError(s"$ctorName: expected ${ctorFieldNames.length} arguments")
            new SchemeRecord(tag, typeName, vals.toArray)
        )
        val pred = SchemeNativeProc(
          predName,
          vals =>
            if vals.length != 1 then throw new EvalError(s"$predName: expected 1 argument")
            vals.head match
              case r: SchemeRecord if r.tag eq tag => SchemeBool(true)
              case _                               => SchemeBool(false)
        )
        val bindings: List[(String, SchemeValue)] =
          (ctorName, ctor) :: (predName, pred) :: accessors.map { (name, idx) =>
            val acc = SchemeNativeProc(
              name,
              vals =>
                if vals.length != 1 then throw new EvalError(s"$name: expected 1 argument")
                vals.head match
                  case r: SchemeRecord if r.tag eq tag => r.fields(idx)
                  case other =>
                    throw new EvalError(s"$name: not a $typeName: ${other.display}")
            )
            (name, acc)
          }
        val newEnv   = bindings.foldLeft(env) { case (e, (n, v)) => e.extend(n, v) }
        val updatedK = Evaluator.updateSeqEnv(k, newEnv)
        ReturnS(SchemeVoid, updatedK, out)
      case _ => throw new EvalError("define-record-type: bad syntax")
