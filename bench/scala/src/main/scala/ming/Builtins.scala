package ming

import SchemeValue.*

/** Pure builtin function implementations for the Scheme interpreter. */
object Builtins:

  private val outputBuffer: ThreadLocal[StringBuilder] =
    ThreadLocal.withInitial(() => new StringBuilder)

  def appendOutput(s: String): Unit =
    outputBuffer.get().append(s)

  def captureOutput[A](body: => A): (A, String) =
    val buf = outputBuffer.get()
    buf.setLength(0)
    val result = body
    val output = buf.toString
    buf.setLength(0)
    (result, output)

  def isTruthy(v: SchemeValue): Boolean = v match
    case SBoolean(false) => false
    case _               => true

  def asInteger(v: SchemeValue): Long = v match
    case SInteger(n) => n
    case other       => throw new EvalError(s"expected number, got ${other.display}")

  def evalAdd(args: List[SchemeValue]): SchemeValue =
    SInteger(args.map(asInteger).sum)

  def evalSub(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil           => throw new EvalError("-: requires at least 1 argument")
      case single :: Nil => SInteger(-asInteger(single))
      case first :: rest =>
        val firstVal = asInteger(first)
        SInteger(rest.foldLeft(firstVal)((acc, a) => acc - asInteger(a)))

  def evalMul(args: List[SchemeValue]): SchemeValue =
    SInteger(args.map(asInteger).product)

  def evalDiv(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case single :: Nil =>
        val v = asInteger(single)
        if v == 0 then throw new EvalError("division by zero")
        SInteger(1 / v)
      case first :: rest =>
        val firstVal = asInteger(first)
        SInteger(rest.foldLeft(firstVal) { (acc, a) =>
          val v = asInteger(a)
          if v == 0 then throw new EvalError("division by zero")
          acc / v
        })

  def evalCompare(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    val vals = args.map(asInteger)
    SBoolean(vals.zip(vals.tail).forall((a, b) => cmp(a, b)))

  def evalNot(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SBoolean(!isTruthy(single))
      case _             => throw new EvalError(s"not: requires exactly 1 argument")

  def evalCons(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => SPair(a, b)
      case _             => throw new EvalError("cons: requires exactly 2 arguments")

  def evalCar(args: List[SchemeValue]): SchemeValue =
    args match
      case SPair(a, _) :: Nil => a
      case _                  => throw new EvalError("car: requires a pair")

  def evalCdr(args: List[SchemeValue]): SchemeValue =
    args match
      case SPair(_, d) :: Nil => d
      case _                  => throw new EvalError("cdr: requires a pair")

  def evalNullPred(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SBoolean(single == SNil)
      case _             => throw new EvalError("null?: requires exactly 1 argument")

  def evalListBuiltin(args: List[SchemeValue]): SchemeValue =
    args.foldRight(SNil: SchemeValue)((e, acc) => SPair(e, acc))

  def evalLength(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SInteger(listLength(single))
      case _             => throw new EvalError("length: requires exactly 1 argument")

  @scala.annotation.tailrec
  private def listLength(v: SchemeValue, acc: Long = 0): Long = v match
    case SNil        => acc
    case SPair(_, d) => listLength(d, acc + 1)
    case _           => throw new EvalError("length: not a proper list")

  def evalAppend(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => SNil
      case last :: Nil => last
      case head :: rest =>
        val restResult = evalAppend(rest)
        appendTwo(head, restResult)

  private def appendTwo(a: SchemeValue, b: SchemeValue): SchemeValue =
    a match
      case SNil        => b
      case SPair(h, t) => SPair(h, appendTwo(t, b))
      case _           => throw new EvalError("append: not a proper list")

  def evalDisplay(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil =>
        appendOutput(single.displayForm)
        SVoid
      case _ => throw new EvalError("display: requires exactly 1 argument")

  def evalWrite(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil =>
        appendOutput(single.display)
        SVoid
      case _ => throw new EvalError("write: requires exactly 1 argument")

  def evalNewline(args: List[SchemeValue]): SchemeValue =
    if args.nonEmpty then throw new EvalError("newline: requires 0 arguments")
    appendOutput("\n")
    SVoid

  def evalStringAppend(args: List[SchemeValue]): SchemeValue =
    val parts = args.map {
      case SString(s) => s.toString
      case other =>
        throw new EvalError(
          s"string-append: expected string, got ${other.display}"
        )
    }
    SchemeValue.makeString(parts.mkString)

  def evalStringLength(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: Nil => SInteger(s.length.toLong)
      case _ =>
        throw new EvalError(
          "string-length: requires exactly 1 string argument"
        )

  def evalSubstring(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: SInteger(start) :: SInteger(end) :: Nil =>
        SchemeValue.makeString(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring: requires (string start end)")

  def evalStringToNumber(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: Nil =>
        s.toString.toLongOption match
          case Some(n) => SInteger(n)
          case None    => SBoolean(false)
      case _ =>
        throw new EvalError(
          "string->number: requires exactly 1 string argument"
        )

  def evalNumberToString(args: List[SchemeValue]): SchemeValue =
    args match
      case SInteger(n) :: Nil => SchemeValue.makeString(n.toString)
      case _ =>
        throw new EvalError(
          "number->string: requires exactly 1 number argument"
        )

  def evalSymbolToString(args: List[SchemeValue]): SchemeValue =
    args match
      case SSymbol(n) :: Nil => SchemeValue.makeString(n)
      case _ =>
        throw new EvalError(
          "symbol->string: requires exactly 1 symbol argument"
        )

  def evalStringToSymbol(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: Nil => SSymbol(s.toString)
      case _ =>
        throw new EvalError(
          "string->symbol: requires exactly 1 string argument"
        )

  def evalStringRef(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: SInteger(i) :: Nil => SChar(s.charAt(i.toInt))
      case _ =>
        throw new EvalError("string-ref: requires (string index)")

  def evalStringSet(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: SInteger(i) :: SChar(c) :: Nil =>
        s.setCharAt(i.toInt, c)
        SVoid
      case _ =>
        throw new EvalError("string-set!: requires (string index char)")

  def evalStringCopy(args: List[SchemeValue]): SchemeValue =
    args match
      case SString(s) :: Nil =>
        SchemeValue.makeString(s.toString)
      case _ =>
        throw new EvalError("string-copy: requires exactly 1 string argument")

  def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    args match
      case single :: Nil => SBoolean(pred(single))
      case _ =>
        throw new EvalError("type predicate: requires exactly 1 argument")
