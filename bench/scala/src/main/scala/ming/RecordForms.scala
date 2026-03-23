package ming

import SchemeValue.*

/** Record type definition handling extracted from SpecialForms. */
object RecordForms:

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  private var nextRecordTypeId: Long = 0

  def evalDefineRecordType(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): SchemeValue =
    args match
      case SymbolVal(typeName, _) :: ListVal(constructorSpec, _) :: SymbolVal(predName, _) :: fieldSpecs =>
        val typeId = nextRecordTypeId
        nextRecordTypeId += 1

        // Parse constructor: (make-foo field1 field2 ...)
        val constructorName = constructorSpec.head match
          case SymbolVal(n, _) => n
          case _               => throw new EvalError(posMsg("define-record-type: bad constructor", pos))
        val constructorFields = constructorSpec.tail.map {
          case SymbolVal(n, _) => n
          case _               => throw new EvalError(posMsg("define-record-type: bad constructor field", pos))
        }

        // Parse field specs: (fieldName accessorName) ...
        val fieldDefs = fieldSpecs.map {
          case ListVal(SymbolVal(fname, _) :: SymbolVal(accessor, _) :: Nil, _) =>
            (fname, accessor)
          case _ => throw new EvalError(posMsg("define-record-type: bad field spec", pos))
        }

        // Map field names to indices (based on constructor order)
        val fieldIndex = constructorFields.zipWithIndex.toMap

        // Define constructor
        env.define(
          constructorName,
          BuiltinVal(
            constructorName,
            cArgs =>
              if cArgs.length != constructorFields.length then
                throw new EvalError(
                  s"$constructorName: expected ${constructorFields.length} arguments, got ${cArgs.length}"
                )
              RecordVal(typeId, typeName, cArgs.toArray)
          )
        )

        // Define predicate
        env.define(
          predName,
          BuiltinVal(
            predName,
            pArgs =>
              pArgs match
                case v :: Nil =>
                  v match
                    case RecordVal(tid, _, _) => BoolVal(tid == typeId)
                    case _                    => BoolVal(false)
                case _ => throw new EvalError(s"$predName: expected 1 argument")
          )
        )

        // Define accessors
        fieldDefs.foreach { (fname, accessor) =>
          val idx = fieldIndex.getOrElse(
            fname,
            throw new EvalError(posMsg(s"define-record-type: field $fname not in constructor", pos))
          )
          env.define(
            accessor,
            BuiltinVal(
              accessor,
              aArgs =>
                aArgs match
                  case RecordVal(tid, _, fields) :: Nil =>
                    if tid != typeId then throw new EvalError(s"$accessor: type mismatch")
                    fields(idx)
                  case _ :: Nil => throw new EvalError(s"$accessor: not a $typeName")
                  case _        => throw new EvalError(s"$accessor: expected 1 argument")
            )
          )
        }

        Void
      case _ => throw new EvalError(posMsg("define-record-type: bad syntax", pos))
