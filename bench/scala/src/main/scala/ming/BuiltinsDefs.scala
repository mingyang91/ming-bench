package ming

import SchemeValue.*

/** Extended builtin registration: builds the ext-builtins list for the global environment. */
object BuiltinsDefs:

  /** All extended builtins, concatenated from sub-lists. */
  def all: List[(String, List[SchemeValue] => SchemeValue)] =
    numericBuiltins ++ cxrBuiltins ++ miscBuiltins

  private def numericBuiltins: List[(String, List[SchemeValue] => SchemeValue)] =
    List(
      ("abs", args => BuiltinsExt.absOp(args)),
      ("modulo", args => BuiltinsExt.moduloOp(args)),
      ("remainder", args => BuiltinsExt.remainderOp(args)),
      ("quotient", args => BuiltinsExt.quotientOp(args)),
      ("min", args => BuiltinsExt.minOp(args)),
      ("max", args => BuiltinsExt.maxOp(args)),
      ("expt", args => BuiltinsExt.exptOp(args)),
      ("zero?", args => zeroCheck(args)),
      ("positive?", args => positiveCheck(args)),
      ("negative?", args => negativeCheck(args)),
      ("odd?", args => oddCheck(args)),
      ("even?", args => evenCheck(args)),
      ("exact?", args => Builtins.typeCheck(args, Rational.isExact)),
      ("inexact?", args => Builtins.typeCheck(args, Rational.isInexact)),
      ("integer?", args => Builtins.typeCheck(args, isIntegerVal)),
      ("rational?", args => Builtins.typeCheck(args, isRationalVal)),
      ("gcd", args => gcdOp(args)),
      ("lcm", args => lcmOp(args)),
      ("truncate", args => truncateOp(args)),
      ("round", args => roundOp(args)),
      ("exact->inexact", args => exactToInexactOp(args)),
      ("inexact->exact", args => inexactToExactOp(args)),
      ("numerator", args => numeratorOp(args)),
      ("denominator", args => denominatorOp(args))
    )

  private def cxrBuiltins: List[(String, List[SchemeValue] => SchemeValue)] =
    List(
      ("cadr", args => cxr(args, "da")),
      ("cdar", args => cxr(args, "ad")),
      ("caar", args => cxr(args, "aa")),
      ("cddr", args => cxr(args, "dd")),
      ("caddr", args => cxr(args, "dda")),
      ("cadddr", args => cxr(args, "ddda")),
      ("caaar", args => cxr(args, "aaa")),
      ("caadr", args => cxr(args, "daa")),
      ("cadar", args => cxr(args, "ada")),
      ("cdaar", args => cxr(args, "aad")),
      ("cdadr", args => cxr(args, "dad")),
      ("cddar", args => cxr(args, "add")),
      ("cdddr", args => cxr(args, "ddd")),
      ("caaaar", args => cxr(args, "aaaa")),
      ("caaadr", args => cxr(args, "daaa")),
      ("caadar", args => cxr(args, "adaa")),
      ("caaddr", args => cxr(args, "ddaa")),
      ("cadaar", args => cxr(args, "aada")),
      ("cadadr", args => cxr(args, "dada")),
      ("caddar", args => cxr(args, "adda")),
      ("cdaaar", args => cxr(args, "aaad")),
      ("cdaadr", args => cxr(args, "daad")),
      ("cdadar", args => cxr(args, "adad")),
      ("cdaddr", args => cxr(args, "ddad")),
      ("cddaar", args => cxr(args, "aadd")),
      ("cddadr", args => cxr(args, "dadd")),
      ("cdddar", args => cxr(args, "addd")),
      ("cddddr", args => cxr(args, "dddd"))
    )

  private def miscBuiltins: List[(String, List[SchemeValue] => SchemeValue)] =
    List(
      ("list-ref", args => BuiltinsExt.listRefOp(args)),
      ("list-tail", args => BuiltinsExt.listTailOp(args)),
      ("list?", args => BuiltinsExt.listCheck(args)),
      ("assoc", args => BuiltinsExt.assocOp(args)),
      ("map", args => BuiltinsExt.mapOp(args)),
      ("reverse", args => BuiltinsExt.reverseOp(args)),
      ("member", args => BuiltinsExt.memberOp(args)),
      ("assv", args => assvOp(args)),
      ("for-each", args => BuiltinsExt.forEachOp(args)),
      ("char-alphabetic?", args => BuiltinsCharStr.charAlphabeticCheck(args)),
      ("char-numeric?", args => BuiltinsCharStr.charNumericCheck(args)),
      ("char-upcase", args => BuiltinsCharStr.charUpcaseOp(args)),
      ("char-downcase", args => BuiltinsCharStr.charDowncaseOp(args)),
      ("char=?", args => BuiltinsCharStr.charEqualCheck(args)),
      ("char<?", args => BuiltinsCharStr.charLessCheck(args)),
      ("string=?", args => BuiltinsCharStr.stringEqualCheck(args)),
      ("string<?", args => BuiltinsCharStr.stringLessCheck(args)),
      ("string-ci=?", args => BuiltinsCharStr.stringCiEqualCheck(args)),
      ("string-upcase", args => BuiltinsCharStr.stringUpcaseOp(args)),
      ("string-downcase", args => BuiltinsCharStr.stringDowncaseOp(args)),
      ("string>?", args => BuiltinsCharStr.stringGreaterCheck(args)),
      ("string<=?", args => BuiltinsCharStr.stringLessEqCheck(args)),
      ("string>=?", args => BuiltinsCharStr.stringGreaterEqCheck(args)),
      ("vector", args => BuiltinsVector.vectorOp(args)),
      ("make-vector", args => BuiltinsVector.makeVectorOp(args)),
      ("vector-ref", args => BuiltinsVector.vectorRefOp(args)),
      ("vector-set!", args => BuiltinsVector.vectorSetOp(args)),
      ("vector-length", args => BuiltinsVector.vectorLengthOp(args)),
      ("vector?", args => BuiltinsVector.vectorCheck(args)),
      ("vector->list", args => BuiltinsVector.vectorToListOp(args)),
      ("list->vector", args => BuiltinsVector.listToVectorOp(args)),
      ("string", args => stringOp(args)),
      ("error", args => errorOp(args)),
      ("make-string", args => makeStringOp(args)),
      ("procedure?", args => procedureCheck(args)),
      ("apply", args => applyOp(args)),
      ("syntax->datum", args => syntaxToDatumOp(args)),
      ("datum->syntax", args => datumToSyntaxOp(args))
    )

  // --- Helper methods ---

  private def cxr(args: List[SchemeValue], ops: String): SchemeValue =
    if args.length != 1 then throw new EvalError(s"cxr: requires 1 argument")
    var v = args.head
    var i = 0
    while i < ops.length do
      v = if ops.charAt(i) == 'a' then Builtins.carOp(List(v)) else Builtins.cdrOp(List(v))
      i += 1
    v

  private def isIntegerVal(v: SchemeValue): Boolean = v match
    case _: IntVal => true
    case _         => false

  private def isRationalVal(v: SchemeValue): Boolean = v match
    case _: IntVal | _: RationalVal => true
    case _                          => false

  private def zeroCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n == 0)
      case _                   => throw new EvalError("zero?: expected 1 number")

  private def positiveCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n > 0)
      case _                   => throw new EvalError("positive?: expected 1 number")

  private def negativeCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n < 0)
      case _                   => throw new EvalError("negative?: expected 1 number")

  private def oddCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n % 2 != 0)
      case _                   => throw new EvalError("odd?: expected 1 number")

  private def evenCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n % 2 == 0)
      case _                   => throw new EvalError("even?: expected 1 number")

  private def gcdOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then IntVal(0)
    else
      val result = args
        .map {
          case IntVal(n, _) => math.abs(n)
          case _            => throw new EvalError("gcd: expected integer")
        }
        .reduce { (a, b) =>
          var x = a; var y = b
          while y != 0 do
            val t = y
            y = x % y
            x = t
          x
        }
      IntVal(result)

  private def lcmOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then IntVal(1)
    else
      val nums = args.map {
        case IntVal(n, _) => math.abs(n)
        case _            => throw new EvalError("lcm: expected integer")
      }
      val result = nums.reduce { (a, b) =>
        if a == 0 || b == 0 then 0L
        else
          var x = a; var y = b
          while y != 0 do
            val t = y
            y = x % y
            x = t
          a / x * b
      }
      IntVal(result)

  private def truncateOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil    => IntVal(n)
      case DoubleVal(d, _) :: Nil => IntVal(d.toLong)
      case _                      => throw new EvalError("truncate: expected 1 number")

  private def roundOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil    => IntVal(n)
      case DoubleVal(d, _) :: Nil => IntVal(math.round(d))
      case _                      => throw new EvalError("round: expected 1 number")

  private def exactToInexactOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil if Rational.isNumeric(v) => DoubleVal(Rational.toDouble(v))
      case _                                 => throw new EvalError("exact->inexact: expected 1 number")

  private def inexactToExactOp(args: List[SchemeValue]): SchemeValue =
    args match
      case DoubleVal(d, _) :: Nil          => Rational.doubleToExact(d)
      case v :: Nil if Rational.isExact(v) => v
      case _                               => throw new EvalError("inexact->exact: expected 1 number")

  private def numeratorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil         => IntVal(n)
      case RationalVal(n, _, _) :: Nil => IntVal(n)
      case _                           => throw new EvalError("numerator: expected exact number")

  private def denominatorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(_, _) :: Nil         => IntVal(1)
      case RationalVal(_, d, _) :: Nil => IntVal(d)
      case _                           => throw new EvalError("denominator: expected exact number")

  private def assvOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil =>
        val elems = Builtins.toScalaList(lst)
        elems
          .collectFirst {
            case entry @ MutablePairVal(cells) if Builtins.schemeEqv(key, cells(0)) => entry
            case entry @ ListVal(k :: _, _) if Builtins.schemeEqv(key, k)           => entry
          }
          .getOrElse(BoolVal(false))
      case _ => throw new EvalError("assv: requires 2 arguments")

  private def stringOp(args: List[SchemeValue]): SchemeValue =
    val chars = args.map {
      case CharVal(c, _) => c
      case _             => throw new EvalError("string: expected chars")
    }
    StringVal(String(chars.toArray))

  private def errorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(msg, _) :: rest => throw new EvalError(s"$msg: ${rest.map(_.display).mkString(" ")}")
      case msg :: rest               => throw new EvalError(s"${msg.display}: ${rest.map(_.display).mkString(" ")}")
      case Nil                       => throw new EvalError("error")

  private def makeStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil                  => MutableStringVal(Array.fill(n.toInt)(' '))
      case IntVal(n, _) :: CharVal(c, _) :: Nil => MutableStringVal(Array.fill(n.toInt)(c))
      case _                                    => throw new EvalError("make-string: expected (size [char])")

  private def procedureCheck(args: List[SchemeValue]): SchemeValue =
    Builtins.typeCheck(
      args,
      {
        case _: LambdaVal | _: BuiltinVal | _: ContinuationVal => true
        case _                                                 => false
      }
    )

  private def syntaxToDatumOp(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("syntax->datum: requires 1 argument")
    args.head

  private def datumToSyntaxOp(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("datum->syntax: requires 2 arguments")
    args(1)

  private def applyOp(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("apply: requires at least 2 arguments")
    val proc    = args.head
    val lastArg = args.last
    val tailList = lastArg match
      case ListVal(es, _)    => es
      case MutablePairVal(_) => Builtins.toScalaList(lastArg)
      case _                 => throw new EvalError("apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    Interpreter.applyProc(proc, prefixArgs ++ tailList)
