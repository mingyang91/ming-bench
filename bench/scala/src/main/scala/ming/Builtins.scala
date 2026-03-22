package ming

import SchemeValue.*

/** Built-in procedure implementations. */
object Builtins:

  def evalBuiltin(
    op: String,
    args: List[SchemeValue]
  ): SchemeValue = op match
    case "+"        => SchemeInt(args.map(asInt).sum)
    case "-"        => evalMinus(args)
    case "*"        => SchemeInt(args.map(asInt).product)
    case "/"        => evalDivide(args)
    case "<"        => compareOp(args, _ < _)
    case ">"        => compareOp(args, _ > _)
    case "="        => compareOp(args, _ == _)
    case "<="       => compareOp(args, _ <= _)
    case ">="       => compareOp(args, _ >= _)
    case "cons"     => evalCons(args)
    case "car"      => evalCar(args)
    case "cdr"      => evalCdr(args)
    case "null?"    => evalNullQ(args)
    case "list"     => SchemeList(args)
    case "append"   => evalAppend(args)
    case "length"   => evalLength(args)
    case "string?"  => typeCheck(args, _.isInstanceOf[SchemeString])
    case "number?"  => typeCheck(args, _.isInstanceOf[SchemeInt])
    case "boolean?" => typeCheck(args, _.isInstanceOf[SchemeBool])
    case "pair?"    => evalPairQ(args)
    case "symbol?"  => typeCheck(args, _.isInstanceOf[SchemeSymbol])
    case _          => throw new EvalError(s"unknown procedure: $op")

  private def evalMinus(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
    else if args.length == 1 then SchemeInt(-asInt(args.head))
    else SchemeInt(args.map(asInt).reduceLeft(_ - _))

  private def evalDivide(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
    else SchemeInt(args.map(asInt).reduceLeft(_ / _))

  private def evalCons(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
    args(1) match
      case SchemeList(es) => SchemeList(args.head :: es)
      case other          => SchemeList(List(args.head, other))

  private def evalCar(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("car: expected 1 argument")
    args.head match
      case SchemeList(h :: _) => h
      case other              => throw new EvalError(s"car: not a pair: ${other.display}")

  private def evalCdr(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
    args.head match
      case SchemeList(_ :: t) => SchemeList(t)
      case other              => throw new EvalError(s"cdr: not a pair: ${other.display}")

  private def evalNullQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("null?: expected 1 argument")
    SchemeBool(args.head == SchemeList(Nil))

  private def evalAppend(args: List[SchemeValue]): SchemeValue =
    val combined = args.foldLeft(List.empty[SchemeValue]) { (acc, arg) =>
      arg match
        case SchemeList(es) => acc ++ es
        case other =>
          throw new EvalError(s"append: not a list: ${other.display}")
    }
    SchemeList(combined)

  private def evalLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("length: expected 1 argument")
    args.head match
      case SchemeList(es) => SchemeInt(es.length.toLong)
      case other =>
        throw new EvalError(s"length: not a list: ${other.display}")

  private def evalPairQ(args: List[SchemeValue]): SchemeValue =
    SchemeBool(args.length == 1 && (args.head match
      case SchemeList(_ :: _) => true
      case _                  => false))

  private def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    SchemeBool(args.length == 1 && pred(args.head))

  private def compareOp(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(asInt(args.head), asInt(args(1))))

  def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other =>
      throw new EvalError(s"expected integer, got: ${other.display}")
