package ming

import scala.collection.mutable

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeVectorBuiltins:

  val bindings: List[(String, Value)] = List(
    "vector" -> Value.Builtin(
      "vector",
      args => Value.VectorValue(mutable.ArrayBuffer.from(args))
    ),
    "make-vector" -> Value.Builtin(
      "make-vector",
      args =>
        args match
          case lengthArg :: Nil =>
            makeVector("make-vector", lengthArg, Value.VoidValue)
          case lengthArg :: fillValue :: Nil =>
            makeVector("make-vector", lengthArg, fillValue)
          case _ =>
            throw new EvalError(s"make-vector expected 1 or 2 argument(s), got ${args.length}")
    ),
    "vector-ref" -> Value.Builtin(
      "vector-ref",
      args =>
        requireArgCount("vector-ref", args, 2)
        val elements = requireVector("vector-ref", args.head)
        elements(requireIndex("vector-ref", args(1), elements.length))
    ),
    "vector-set!" -> Value.Builtin(
      "vector-set!",
      args =>
        requireArgCount("vector-set!", args, 3)
        val elements = requireVector("vector-set!", args.head)
        val index    = requireIndex("vector-set!", args(1), elements.length)
        elements(index) = args(2)
        Value.VoidValue
    ),
    "vector-length" -> Value.Builtin(
      "vector-length",
      args =>
        requireArgCount("vector-length", args, 1)
        Value.IntegerValue(BigInt(requireVector("vector-length", args.head).length))
    ),
    "vector->list" -> Value.Builtin(
      "vector->list",
      args =>
        requireArgCount("vector->list", args, 1)
        makeList(requireVector("vector->list", args.head).toList)
    ),
    "list->vector" -> Value.Builtin(
      "list->vector",
      args =>
        requireArgCount("list->vector", args, 1)
        Value.VectorValue(mutable.ArrayBuffer.from(properListElements("list->vector", args.head)))
    )
  )

  private def makeVector(name: String, lengthArg: Value, fillValue: Value): Value =
    val length = requireExactInteger(name, lengthArg)
    if !length.isValidInt || length.signum < 0 then throw new EvalError(s"$name expected a non-negative length")
    Value.VectorValue(mutable.ArrayBuffer.fill(length.toInt)(fillValue))
