package ming

/** Built-in procedure implementations. Each returns (Value, output). */
object Builtins:

  def call(name: String, args: List[Value]): (Value, String) =
    name match
      case "display" => displayOp(args)
      case "write"   => writeOp(args)
      case "newline" => newlineOp(args)
      case _         => (callPure(name, args), "")

  private def displayOp(args: List[Value]): (Value, String) = args match
    case v :: Nil => (Value.Void, v.displayStr)
    case _        => throw new EvalError("display requires 1 argument")

  private def writeOp(args: List[Value]): (Value, String) = args match
    case v :: Nil => (Value.Void, v.display)
    case _        => throw new EvalError("write requires 1 argument")

  private def newlineOp(args: List[Value]): (Value, String) = args match
    case Nil => (Value.Void, "\n")
    case _   => throw new EvalError("newline requires 0 arguments")

  private def callPure(name: String, args: List[Value]): Value =
    name match
      case "+"        => addOp(args)
      case "*"        => mulOp(args)
      case "-"        => minusOp(args)
      case "/"        => divOp(args)
      case "<"        => numCmpOp(args, _ < 0)
      case ">"        => numCmpOp(args, _ > 0)
      case "="        => numCmpOp(args, _ == 0)
      case "<="       => numCmpOp(args, _ <= 0)
      case ">="       => numCmpOp(args, _ >= 0)
      case "not"      => notOp(args)
      case "cons"     => consOp(args)
      case "car"      => carOp(args)
      case "cdr"      => cdrOp(args)
      case "null?"    => nullPred(args)
      case "list"     => listOp(args)
      case "length"   => lengthOp(args)
      case "string?"  => predOp(args, "string?") { case Value.Str(_) => true; case Value.MutStr(_) => true }
      case "number?"  => predOp(args, "number?") { case v if RationalOps.isNumber(v) => true }
      case "integer?" => predOp(args, "integer?") { case v if RationalOps.isInteger(v) => true }
      case "rational?" =>
        predOp(args, "rational?") { case Value.Integer(_) | Value.Rational(_, _) =>
          true
        }
      case "exact?"         => predOp(args, "exact?") { case v if RationalOps.isExact(v) => true }
      case "inexact?"       => predOp(args, "inexact?") { case v if !RationalOps.isExact(v) => true }
      case "exact->inexact" => exactToInexact(args)
      case "inexact->exact" => inexactToExact(args)
      case "numerator"      => numeratorOp(args)
      case "denominator"    => denominatorOp(args)
      case "boolean?"       => predOp(args, "boolean?") { case Value.Bool(_) => true }
      case "pair?"          => predOp(args, "pair?") { case Value.SList(_ :: _) => true; case _: Value.Pair => true }
      case "set-car!"       => setCarOp(args)
      case "set-cdr!"       => setCdrOp(args)
      case "caar"           => carOp(List(carOp(args)))
      case "cadr"           => carOp(List(cdrOp(args)))
      case "cdar"           => cdrOp(List(carOp(args)))
      case "cddr"           => cdrOp(List(cdrOp(args)))
      case "symbol?"        => predOp(args, "symbol?") { case Value.Symbol(_) => true }
      case "char?"          => predOp(args, "char?") { case Value.Char(_) => true }
      case "string-append"  => StringOps.stringAppendOp(args)
      case "string-length"  => StringOps.stringLengthOp(args)
      case "substring"      => StringOps.substringOp(args)
      case "string->number" => StringOps.stringToNumberOp(args)
      case "number->string" => StringOps.numberToStringOp(args)
      case "symbol->string" => StringOps.symbolToStringOp(args)
      case "string->symbol" => StringOps.stringToSymbolOp(args)
      case "string-ref"     => StringOps.stringRefOp(args)
      case "string-copy"    => StringOps.stringCopyOp(args)
      case "string-set!"    => StringOps.stringSetOp(args)
      case "string->list"   => StringOps.stringToListOp(args)
      case "list->string"   => StringOps.listToStringOp(args)
      case "char->integer"  => StringOps.charToIntegerOp(args)
      case "integer->char"  => StringOps.integerToCharOp(args)
      case "equal?"         => equalPred(args)
      case "eqv?"           => eqvPred(args)
      case "eq?"            => eqPred(args)
      case "vector"         => Value.Vec(args.toArray)
      case "make-vector"    => VectorOps.makeVectorOp(args)
      case "vector-ref"     => VectorOps.vectorRefOp(args)
      case "vector-set!"    => VectorOps.vectorSetOp(args)
      case "vector-length"  => VectorOps.vectorLengthOp(args)
      case "vector?"        => predOp(args, "vector?") { case _: Value.Vec => true }
      case "vector->list"   => VectorOps.vectorToListOp(args)
      case "list->vector"   => VectorOps.listToVectorOp(args)
      case "reverse"        => VectorOps.reverseOp(args)
      case "syntax->datum"  => syntaxToDatum(args)
      case "datum->syntax"  => datumToSyntax(args)
      case other            => NumCharOps.callPure(other, args)

  private def predOp(args: List[Value], name: String)(pf: PartialFunction[Value, Boolean]): Value =
    args match
      case v :: Nil => Value.Bool(pf.applyOrElse(v, (_: Value) => false))
      case _        => throw new EvalError(s"$name requires 1 argument")

  private def asLong(v: Value): Long = v match
    case Value.Integer(n)         => n
    case Value.Rational(num, den) => num / den
    case Value.Float(d)           => d.toLong
    case _                        => throw new EvalError(s"expected number, got: ${v.display}")

  private def addOp(args: List[Value]): Value =
    if RationalOps.hasInexact(args) then Value.Float(args.foldLeft(0.0)((acc, v) => acc + RationalOps.toDouble(v)))
    else
      args.foldLeft(Value.Integer(0L): Value) { (acc, v) =>
        RationalOps.addExact(RationalOps.toRational(acc), RationalOps.toRational(v))
      }

  private def mulOp(args: List[Value]): Value =
    if RationalOps.hasInexact(args) then Value.Float(args.foldLeft(1.0)((acc, v) => acc * RationalOps.toDouble(v)))
    else
      args.foldLeft(Value.Integer(1L): Value) { (acc, v) =>
        RationalOps.mulExact(RationalOps.toRational(acc), RationalOps.toRational(v))
      }

  private def minusOp(args: List[Value]): Value = args match
    case Nil => throw new EvalError("- requires at least 1 argument")
    case single :: Nil =>
      single match
        case Value.Integer(n)         => Value.Integer(-n)
        case Value.Rational(num, den) => Value.Rational(-num, den)
        case Value.Float(d)           => Value.Float(-d)
        case _                        => throw new EvalError(s"expected number, got: ${single.display}")
    case head :: tail =>
      if RationalOps.hasInexact(args) then
        Value.Float(tail.foldLeft(RationalOps.toDouble(head))((a, v) => a - RationalOps.toDouble(v)))
      else
        tail.foldLeft(head) { (acc, v) =>
          RationalOps.subExact(RationalOps.toRational(acc), RationalOps.toRational(v))
        }

  private def divOp(args: List[Value]): Value = args match
    case Nil      => throw new EvalError("/ requires at least 1 argument")
    case _ :: Nil => throw new EvalError("/ requires at least 2 arguments")
    case head :: tail =>
      if RationalOps.hasInexact(args) then
        Value.Float(tail.foldLeft(RationalOps.toDouble(head)) { (a, v) =>
          val d = RationalOps.toDouble(v)
          if d == 0.0 then throw new EvalError("division by zero") else a / d
        })
      else
        tail.foldLeft(head) { (acc, v) =>
          RationalOps.divExact(RationalOps.toRational(acc), RationalOps.toRational(v))
        }

  private def numCmpOp(args: List[Value], pred: Int => Boolean): Value =
    require(args.length == 2, "comparison requires 2 arguments")
    Value.Bool(pred(RationalOps.compareNumeric(args.head, args(1))))

  private def notOp(args: List[Value]): Value =
    require(args.length == 1, "not requires 1 argument")
    Value.Bool(args.head match
      case Value.Bool(false) => true;
      case _                 => false)

  private def consOp(args: List[Value]): Value = args match
    case head :: cdr :: Nil => Value.Pair(Array(head, cdr))
    case _                  => throw new EvalError("cons requires 2 arguments")

  private def carOp(args: List[Value]): Value = args match
    case Value.SList(head :: _) :: Nil => head
    case Value.Pair(cell) :: Nil       => cell(0)
    case Value.SList(Nil) :: Nil       => throw new EvalError("car: empty list")
    case _ :: Nil                      => throw new EvalError("car: not a pair")
    case _                             => throw new EvalError("car requires 1 argument")

  private def cdrOp(args: List[Value]): Value = args match
    case Value.SList(_ :: tail) :: Nil => Value.SList(tail)
    case Value.Pair(cell) :: Nil       => cell(1)
    case Value.SList(Nil) :: Nil       => throw new EvalError("cdr: empty list")
    case _ :: Nil                      => throw new EvalError("cdr: not a pair")
    case _                             => throw new EvalError("cdr requires 1 argument")

  private def listOp(args: List[Value]): Value =
    args.foldRight(Value.SList(Nil): Value)((elem, acc) => Value.Pair(Array(elem, acc)))

  private def setCarOp(args: List[Value]): Value = args match
    case Value.Pair(cell) :: v :: Nil => cell(0) = v; Value.Void
    case _ :: _ :: Nil                => throw new EvalError("set-car!: not a mutable pair")
    case _                            => throw new EvalError("set-car! requires 2 arguments")

  private def setCdrOp(args: List[Value]): Value = args match
    case Value.Pair(cell) :: v :: Nil => cell(1) = v; Value.Void
    case _ :: _ :: Nil                => throw new EvalError("set-cdr!: not a mutable pair")
    case _                            => throw new EvalError("set-cdr! requires 2 arguments")

  private def nullPred(args: List[Value]): Value = args match
    case Value.SList(Nil) :: Nil => Value.Bool(true)
    case _ :: Nil                => Value.Bool(false)
    case _                       => throw new EvalError("null? requires 1 argument")

  private def lengthOp(args: List[Value]): Value = args match
    case lst :: Nil =>
      NumCharOps.toScalaList(lst) match
        case Some(elems) => Value.Integer(elems.length.toLong)
        case None        => throw new EvalError("length: not a list")
    case _ => throw new EvalError("length requires 1 argument")

  private def exactToInexact(args: List[Value]): Value = args match
    case v :: Nil => Value.Float(RationalOps.toDouble(v))
    case _        => throw new EvalError("exact->inexact requires 1 argument")

  private def inexactToExact(args: List[Value]): Value = args match
    case Value.Float(d) :: Nil              => floatToExact(d)
    case v :: Nil if RationalOps.isExact(v) => v
    case _ :: Nil                           => throw new EvalError("inexact->exact: not a number")
    case _                                  => throw new EvalError("inexact->exact requires 1 argument")

  private def floatToExact(d: Double): Value =
    if d == d.floor && !d.isInfinite && math.abs(d) < Long.MaxValue.toDouble then Value.Integer(d.toLong)
    else
      val bits = java.lang.Double.doubleToLongBits(d)
      val sign = if (bits >>> 63) != 0 then -1L else 1L
      val exp  = ((bits >>> 52) & 0x7ffL).toInt - 1023
      val mant =
        if exp == -1023 then (bits & 0xfffffffffffffL) << 1
        else (bits & 0xfffffffffffffL) | (1L << 52)
      val shift = 52 - exp
      if shift >= 0 then RationalOps.makeRational(sign * mant, 1L << shift)
      else RationalOps.makeRational(sign * mant * (1L << (-shift)), 1L)

  private def numeratorOp(args: List[Value]): Value = args match
    case Value.Integer(n) :: Nil       => Value.Integer(n)
    case Value.Rational(num, _) :: Nil => Value.Integer(num)
    case _ :: Nil                      => throw new EvalError("numerator: not a rational number")
    case _                             => throw new EvalError("numerator requires 1 argument")

  private def denominatorOp(args: List[Value]): Value = args match
    case Value.Integer(_) :: Nil       => Value.Integer(1)
    case Value.Rational(_, den) :: Nil => Value.Integer(den)
    case _ :: Nil                      => throw new EvalError("denominator: not a rational number")
    case _                             => throw new EvalError("denominator requires 1 argument")

  private[ming] def deepEqual(a: Value, b: Value): Boolean = (a, b) match
    case (Value.Integer(x), Value.Integer(y))             => x == y
    case (Value.Rational(a1, b1), Value.Rational(a2, b2)) => a1 == a2 && b1 == b2
    case (Value.Float(x), Value.Float(y))                 => x == y
    case (Value.Bool(x), Value.Bool(y))                   => x == y
    case (Value.Str(x), Value.Str(y))                     => x == y
    case (Value.MutStr(x), Value.MutStr(y))               => new String(x) == new String(y)
    case (Value.Str(x), Value.MutStr(y))                  => x == new String(y)
    case (Value.MutStr(x), Value.Str(y))                  => new String(x) == y
    case (Value.Symbol(x), Value.Symbol(y))               => x == y
    case (Value.Char(x), Value.Char(y))                   => x == y
    case (Value.Pair(c1), Value.Pair(c2))                 => deepEqual(c1(0), c2(0)) && deepEqual(c1(1), c2(1))
    case (Value.SList(xs), Value.SList(ys)) => xs.length == ys.length && xs.zip(ys).forall((a, b) => deepEqual(a, b))
    case (Value.Vec(xs), Value.Vec(ys))     => xs.length == ys.length && xs.zip(ys).forall((a, b) => deepEqual(a, b))
    case (Value.Void, Value.Void)           => true
    case _                                  => deepEqualList(a, b)

  private def deepEqualList(a: Value, b: Value): Boolean =
    (NumCharOps.toScalaList(a), NumCharOps.toScalaList(b)) match
      case (Some(xs), Some(ys)) => xs.length == ys.length && xs.zip(ys).forall(deepEqual(_, _))
      case _                    => false

  private def equalPred(args: List[Value]): Value = args match
    case a :: b :: Nil => Value.Bool(deepEqual(a, b))
    case _             => throw new EvalError("equal? requires 2 arguments")

  private def eqvPred(args: List[Value]): Value = args match
    case a :: b :: Nil => Value.Bool(eqvCheck(a, b))
    case _             => throw new EvalError("eqv? requires 2 arguments")

  private def eqvCheck(a: Value, b: Value): Boolean = (a, b) match
    case (Value.Integer(x), Value.Integer(y)) => x == y
    case (Value.Bool(x), Value.Bool(y))       => x == y
    case (Value.Symbol(x), Value.Symbol(y))   => x == y
    case (Value.Char(x), Value.Char(y))       => x == y
    case (Value.SList(Nil), Value.SList(Nil)) => true
    case (Value.Void, Value.Void)             => true
    case _                                    => a eq b

  private def eqPred(args: List[Value]): Value = args match
    case a :: b :: Nil => Value.Bool(eqvCheck(a, b))
    case _             => throw new EvalError("eq? requires 2 arguments")

  private def syntaxToDatum(args: List[Value]): Value = args match
    case v :: Nil => v
    case _        => throw new EvalError("syntax->datum requires 1 argument")

  private def datumToSyntax(args: List[Value]): Value = args match
    case _ :: datum :: Nil => datum
    case _                 => throw new EvalError("datum->syntax requires 2 arguments")
