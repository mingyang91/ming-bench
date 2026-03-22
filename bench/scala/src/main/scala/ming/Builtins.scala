package ming

import SchemeValue.*

object Builtins:

  def applyBuiltin(
    name: String,
    args: List[SchemeValue]
  ): SchemeValue =
    name match
      case "+" => arithOp(args, 0L, _ + _)
      case "-" =>
        args match
          case Nil              => throw new EvalError("-: need at least 1 argument")
          case IntVal(n) :: Nil => IntVal(-n)
          case _                => arithOp(args.tail, asInt(args.head), _ - _)
      case "*" => arithOp(args, 1L, _ * _)
      case "/" =>
        args match
          case Nil => throw new EvalError("/: need at least 1 argument")
          case _ =>
            val result = args.tail.foldLeft(asInt(args.head)) { (acc, v) =>
              val d = asInt(v)
              if d == 0 then throw new EvalError("division by zero")
              else acc / d
            }
            IntVal(result)
      case "<"  => cmpOp(args, _ < _)
      case ">"  => cmpOp(args, _ > _)
      case "="  => cmpOp(args, _ == _)
      case "<=" => cmpOp(args, _ <= _)
      case ">=" => cmpOp(args, _ >= _)
      case "not" =>
        args match
          case v :: Nil => BoolVal(!v.isTruthy)
          case _        => throw new EvalError("not: expects 1 argument")
      case "cons" =>
        args match
          case a :: b :: Nil => PairVal(a, b)
          case _             => throw new EvalError("cons: expects 2 arguments")
      case "car" =>
        args match
          case PairVal(a, _) :: Nil   => a
          case ListVal(h :: _) :: Nil => h
          case _                      => throw new EvalError("car: expects a pair")
      case "cdr" =>
        args match
          case PairVal(_, d) :: Nil   => d
          case ListVal(_ :: t) :: Nil => listToPairs(t)
          case _                      => throw new EvalError("cdr: expects a pair")
      case "null?" =>
        args match
          case v :: Nil => BoolVal(isNull(v))
          case _        => throw new EvalError("null?: expects 1 argument")
      case "list" => listToPairs(args)
      case "length" =>
        args match
          case v :: Nil => IntVal(pairLength(v))
          case _        => throw new EvalError("length: expects 1 argument")
      case "pair?" =>
        args match
          case v :: Nil => BoolVal(isPair(v))
          case _        => throw new EvalError("pair?: expects 1 argument")
      case "number?" =>
        args match
          case (_: IntVal) :: Nil => BoolVal(true)
          case _ :: Nil           => BoolVal(false)
          case _                  => throw new EvalError("number?: expects 1 argument")
      case "boolean?" =>
        args match
          case (_: BoolVal) :: Nil => BoolVal(true)
          case _ :: Nil            => BoolVal(false)
          case _                   => throw new EvalError("boolean?: expects 1 argument")
      case "string?" =>
        args match
          case (_: StringVal) :: Nil => BoolVal(true)
          case _ :: Nil              => BoolVal(false)
          case _                     => throw new EvalError("string?: expects 1 argument")
      case "symbol?" =>
        args match
          case (_: SymbolVal) :: Nil => BoolVal(true)
          case _ :: Nil              => BoolVal(false)
          case _                     => throw new EvalError("symbol?: expects 1 argument")
      case "append" =>
        args match
          case a :: b :: Nil => appendLists(a, b)
          case _             => throw new EvalError("append: expects 2 arguments")
      case _ => throw new EvalError(s"unknown procedure: $name")

  def isNull(v: SchemeValue): Boolean = v match
    case ListVal(Nil) => true
    case _            => false

  def isPair(v: SchemeValue): Boolean = v match
    case _: PairVal      => true
    case ListVal(_ :: _) => true
    case _               => false

  def listToPairs(elements: List[SchemeValue]): SchemeValue =
    elements.foldRight(ListVal(Nil): SchemeValue)((el, acc) => PairVal(el, acc))

  private def pairLength(v: SchemeValue): Long = v match
    case ListVal(Nil)    => 0
    case ListVal(es)     => es.length.toLong
    case PairVal(_, cdr) => 1 + pairLength(cdr)
    case _               => throw new EvalError("length: not a proper list")

  private def appendLists(a: SchemeValue, b: SchemeValue): SchemeValue =
    a match
      case ListVal(Nil)    => b
      case PairVal(h, t)   => PairVal(h, appendLists(t, b))
      case ListVal(h :: t) => PairVal(h, appendLists(listToPairs(t), b))
      case _               => throw new EvalError("append: not a proper list")

  private def asInt(v: SchemeValue): Long = v match
    case IntVal(n) => n
    case other     => throw new EvalError(s"expected number, got: ${other.display}")

  private def arithOp(
    args: List[SchemeValue],
    init: Long,
    op: (Long, Long) => Long
  ): SchemeValue =
    IntVal(args.foldLeft(init)((acc, v) => op(acc, asInt(v))))

  private def cmpOp(
    args: List[SchemeValue],
    op: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(op(asInt(a), asInt(b)))
      case _             => throw new EvalError("comparison expects 2 arguments")
