package ming

import SchemeValue.*

object BuiltinsL15:

  def applyL15(
    name: String,
    args: List[SchemeValue]
  ): (SchemeValue, String) =
    name match
      case "vector"        => (vectorCreate(args), "")
      case "make-vector"   => (makeVector(args), "")
      case "vector-ref"    => (vectorRef(args), "")
      case "vector-set!"   => (vectorSet(args), "")
      case "vector-length" => (vectorLength(args), "")
      case "vector?"       => (vectorPred(args), "")
      case "vector->list"  => (vectorToList(args), "")
      case "list->vector"  => (listToVector(args), "")
      case "set-car!"      => (setCar(args), "")
      case "set-cdr!"      => (setCdr(args), "")
      case "error"         => errorOp(args)
      case "procedure?"    => (procedurePred(args), "")
      case _               => throw new EvalError(s"unknown procedure: $name")

  private def vectorCreate(args: List[SchemeValue]): SchemeValue =
    VectorVal(args.toArray)

  private def makeVector(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n) :: Nil =>
        VectorVal(Array.fill(n.toInt)(IntVal(0L)))
      case IntVal(n) :: fill :: Nil =>
        VectorVal(Array.fill(n.toInt)(fill))
      case _ => throw new EvalError("make-vector: expects size and optional fill")

  private def vectorRef(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es) :: IntVal(i) :: Nil =>
        if i < 0 || i >= es.length then throw new EvalError("vector-ref: index out of range")
        es(i.toInt)
      case _ => throw new EvalError("vector-ref: expects vector and index")

  private def vectorSet(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es) :: IntVal(i) :: v :: Nil =>
        if i < 0 || i >= es.length then throw new EvalError("vector-set!: index out of range")
        es(i.toInt) = v
        Void
      case _ => throw new EvalError("vector-set!: expects vector, index, and value")

  private def vectorLength(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es) :: Nil => IntVal(es.length.toLong)
      case _                    => throw new EvalError("vector-length: expects vector")

  private def vectorPred(args: List[SchemeValue]): SchemeValue =
    args match
      case (_: VectorVal) :: Nil => BoolVal(true)
      case _ :: Nil              => BoolVal(false)
      case _                     => throw new EvalError("vector?: expects 1 argument")

  private def vectorToList(args: List[SchemeValue]): SchemeValue =
    args match
      case VectorVal(es) :: Nil => Builtins.listToPairs(es.toList)
      case _                    => throw new EvalError("vector->list: expects vector")

  private def listToVector(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        val elems = InterpreterUtils.schemeListToList(v)
        VectorVal(elems.toArray)
      case _ => throw new EvalError("list->vector: expects list")

  private def setCar(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(data) :: v :: Nil =>
        data(0) = v
        Void
      case _ => throw new EvalError("set-car!: expects a mutable pair and a value")

  private def setCdr(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(data) :: v :: Nil =>
        data(1) = v
        Void
      case _ => throw new EvalError("set-cdr!: expects a mutable pair and a value")

  private def errorOp(args: List[SchemeValue]): Nothing =
    val msg = args.map(_.display).mkString(" ")
    throw new EvalError(s"error: $msg")

  private def procedurePred(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        v match
          case _: LambdaVal       => BoolVal(true)
          case _: ContinuationVal => BoolVal(true)
          case SymbolVal(name, _) =>
            BoolVal(isBuiltinProcedure(name))
          case _ => BoolVal(false)
      case _ => throw new EvalError("procedure?: expects 1 argument")

  private def isBuiltinProcedure(name: String): Boolean =
    val builtins = Set(
      "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
      "not", "cons", "car", "cdr", "null?", "list", "length",
      "pair?", "number?", "boolean?", "string?", "symbol?", "char?",
      "append", "display", "write", "newline",
      "string-append", "string-length", "substring",
      "string->number", "number->string", "symbol->string", "string->symbol",
      "string-ref", "string-copy", "string-set!",
      "eq?", "eqv?", "equal?",
      "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
      "zero?", "positive?", "negative?", "odd?", "even?",
      "list-ref", "list-tail", "list?", "assoc",
      "map", "for-each", "apply", "call/cc", "call-with-current-continuation",
      "vector", "make-vector", "vector-ref", "vector-set!",
      "vector-length", "vector?", "vector->list", "list->vector",
      "error", "procedure?"
    )
    builtins.contains(name)
