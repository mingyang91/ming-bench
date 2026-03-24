package ming

import Evaluator.Val
import Evaluator.Val.*

/** Handles define-record-type evaluation. */
object Records:

  /** (define-record-type <name> (constructor field ...) predicate (field accessor) ...) */
  def evalDefineRecordType(rest: Val, env: Env, error: String => Nothing): Val =
    val parts = Evaluator.toList(rest)
    if parts.length < 3 then error("bad define-record-type syntax")
    val typeName = parts(0) match
      case Symbol(n) => n
      case _         => error("define-record-type: expected type name")
    val (ctorName, ctorFields) = parseConstructor(parts(1), error)
    val predName = parts(2) match
      case Symbol(n) => n
      case _         => error("define-record-type: expected predicate name")
    val fieldSpecs = parts.drop(3).map {
      case Pair(Symbol(fieldName), Pair(Symbol(accessorName), Nil)) =>
        (fieldName, accessorName)
      case _ => error("define-record-type: bad field spec")
    }
    val fieldIndex = ctorFields.zipWithIndex.toMap
    for (fn, _) <- fieldSpecs do if !fieldIndex.contains(fn) then error(s"define-record-type: unknown field $fn")
    defineConstructor(env, ctorName, ctorFields, typeName, error)
    definePredicate(env, predName, typeName, error)
    defineAccessors(env, fieldSpecs, fieldIndex, typeName, error)
    Void

  private def parseConstructor(
    spec: Val,
    error: String => Nothing
  ): (String, List[String]) =
    spec match
      case Pair(Symbol(name), fieldList) =>
        val fields = Evaluator.toList(fieldList).map {
          case Symbol(f) => f
          case _         => error("define-record-type: bad constructor field")
        }
        (name, fields)
      case _ => error("define-record-type: bad constructor")

  private def defineConstructor(
    env: Env,
    ctorName: String,
    ctorFields: List[String],
    typeName: String,
    error: String => Nothing
  ): Unit =
    env.define(
      ctorName,
      Builtin { args =>
        if args.length != ctorFields.length then
          error(s"$ctorName: expected ${ctorFields.length} arguments, got ${args.length}")
        Record(typeName, args.toArray)
      }
    )

  private def definePredicate(
    env: Env,
    predName: String,
    typeName: String,
    error: String => Nothing
  ): Unit =
    env.define(
      predName,
      Builtin {
        case List(Record(tag, _)) => Bool(tag == typeName)
        case List(_)              => Bool(false)
        case args                 => error(s"$predName: expected 1 argument, got ${args.length}")
      }
    )

  private def defineAccessors(
    env: Env,
    fieldSpecs: List[(String, String)],
    fieldIndex: Map[String, Int],
    typeName: String,
    error: String => Nothing
  ): Unit =
    for (fieldName, accessorName) <- fieldSpecs do
      val idx = fieldIndex(fieldName)
      env.define(
        accessorName,
        Builtin {
          case List(Record(tag, fields)) if tag == typeName => fields(idx)
          case List(other) => error(s"$accessorName: expected $typeName, got ${Display.write(other)}")
          case args        => error(s"$accessorName: expected 1 argument, got ${args.length}")
        }
      )
