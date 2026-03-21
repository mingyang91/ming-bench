package ming

import Evaluator.{Done, EvalResult}

/** define-record-type implementation (R7RS). */
private[ming] object Records:

  def evalDefineRecordType(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    args match
      case Value.Symbol(typeName, _) ::
          constructorSpec ::
          Value.Symbol(predName, _) ::
          fieldSpecs =>
        val (ctorName, ctorParams) = parseConstructor(constructorSpec)
        val fields                 = fieldSpecs.map(parseFieldSpec)
        val fieldNames             = fields.map(_._1)
        val accessorNames          = fields.map(_._2)
        val typeTag                = new AnyRef
        val paramIndexMap          = ctorParams.zipWithIndex.toMap
        val fieldIndexMap          = fieldNames.zipWithIndex.toMap
        val ctor = Value.NativeProcVal(
          ctorName,
          ctorArgs =>
            if ctorArgs.length != ctorParams.length then
              throw new EvalError(
                s"$ctorName: expected ${ctorParams.length} arguments, got ${ctorArgs.length}"
              )
            val arr = new Array[Value](fieldNames.length)
            ctorParams.zipWithIndex.foreach { case (param, i) =>
              arr(fieldIndexMap(param)) = ctorArgs(i)
            }
            Value.RecordVal(typeTag, fieldNames, arr)
        )
        val pred = Value.NativeProcVal(
          predName,
          predArgs =>
            predArgs match
              case v :: Nil =>
                v match
                  case Value.RecordVal(tag, _, _) =>
                    Value.BoolVal(tag eq typeTag)
                  case _ => Value.BoolVal(false)
              case _ =>
                throw new EvalError(s"$predName: expected 1 argument")
        )
        val envWithCtorPred = env
          .define(ctorName, ctor)
          .define(predName, pred)
        val finalEnv = fields.foldLeft(envWithCtorPred) { case (e, (fName, accessorName)) =>
          val idx = fieldIndexMap(fName)
          val accessor = Value.NativeProcVal(
            accessorName,
            accArgs =>
              accArgs match
                case Value.RecordVal(tag, _, flds) :: Nil =>
                  if !(tag eq typeTag) then
                    throw new EvalError(
                      s"$accessorName: not a $typeName"
                    )
                  flds(idx)
                case _ :: Nil =>
                  throw new EvalError(
                    s"$accessorName: not a $typeName"
                  )
                case _ =>
                  throw new EvalError(
                    s"$accessorName: expected 1 argument"
                  )
          )
          e.define(accessorName, accessor)
        }
        Done(Value.VoidVal, finalEnv, out)
      case _ =>
        throw EvalError.withPos("bad define-record-type syntax", pos)

  private def parseConstructor(
    spec: Value
  ): (String, List[String]) =
    val elems = Evaluator.toList(spec)
    elems match
      case Value.Symbol(name, _) :: params =>
        val paramNames = params.map {
          case Value.Symbol(n, _) => n
          case other =>
            throw new EvalError(
              s"bad constructor param: ${other.display}"
            )
        }
        (name, paramNames)
      case _ =>
        throw new EvalError("bad constructor specification")

  private def parseFieldSpec(
    spec: Value
  ): (String, String) =
    Evaluator.toList(spec) match
      case Value.Symbol(fieldName, _) ::
          Value.Symbol(accessorName, _) :: Nil =>
        (fieldName, accessorName)
      case _ =>
        throw new EvalError(
          s"bad field specification: ${spec.display}"
        )
