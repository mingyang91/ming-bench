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
      ("zero?", args => BuiltinsDefsNumeric.zeroCheck(args)),
      ("positive?", args => BuiltinsDefsNumeric.positiveCheck(args)),
      ("negative?", args => BuiltinsDefsNumeric.negativeCheck(args)),
      ("odd?", args => BuiltinsDefsNumeric.oddCheck(args)),
      ("even?", args => BuiltinsDefsNumeric.evenCheck(args)),
      ("exact?", args => Builtins.typeCheck(args, Rational.isExact)),
      ("inexact?", args => Builtins.typeCheck(args, Rational.isInexact)),
      ("integer?", args => Builtins.typeCheck(args, BuiltinsDefsNumeric.isIntegerVal)),
      ("rational?", args => Builtins.typeCheck(args, BuiltinsDefsNumeric.isRationalVal)),
      ("gcd", args => BuiltinsDefsNumeric.gcdOp(args)),
      ("lcm", args => BuiltinsDefsNumeric.lcmOp(args)),
      ("truncate", args => BuiltinsDefsNumeric.truncateOp(args)),
      ("round", args => BuiltinsDefsNumeric.roundOp(args)),
      ("exact->inexact", args => BuiltinsDefsNumeric.exactToInexactOp(args)),
      ("inexact->exact", args => BuiltinsDefsNumeric.inexactToExactOp(args)),
      ("numerator", args => BuiltinsDefsNumeric.numeratorOp(args)),
      ("denominator", args => BuiltinsDefsNumeric.denominatorOp(args))
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
      ("memq", args => memqOp(args)),
      ("memv", args => memvOp(args)),
      ("assq", args => assqOp(args)),
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

  private def memqOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil =>
        var cur = lst
        while true do
          cur match
            case ListVal(Nil, _)                                           => return BoolVal(false)
            case ListVal(h :: _, _) if Builtins.schemeEq(key, h)           => return cur
            case ListVal(_ :: t, p)                                        => cur = ListVal(t, p)
            case MutablePairVal(cells) if Builtins.schemeEq(key, cells(0)) => return cur
            case MutablePairVal(cells)                                     => cur = cells(1)
            case _                                                         => return BoolVal(false)
        BoolVal(false)
      case _ => throw new EvalError("memq: requires 2 arguments")

  private def memvOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil =>
        var cur = lst
        while true do
          cur match
            case ListVal(Nil, _)                                            => return BoolVal(false)
            case ListVal(h :: _, _) if Builtins.schemeEqv(key, h)           => return cur
            case ListVal(_ :: t, p)                                         => cur = ListVal(t, p)
            case MutablePairVal(cells) if Builtins.schemeEqv(key, cells(0)) => return cur
            case MutablePairVal(cells)                                      => cur = cells(1)
            case _                                                          => return BoolVal(false)
        BoolVal(false)
      case _ => throw new EvalError("memv: requires 2 arguments")

  private def assqOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil =>
        val elems = Builtins.toScalaList(lst)
        elems
          .collectFirst {
            case entry @ MutablePairVal(cells) if Builtins.schemeEq(key, cells(0)) => entry
            case entry @ ListVal(k :: _, _) if Builtins.schemeEq(key, k)           => entry
          }
          .getOrElse(BoolVal(false))
      case _ => throw new EvalError("assq: requires 2 arguments")

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
        case _: LambdaVal | _: CaseLambdaVal | _: BuiltinVal | _: ContinuationVal => true
        case _                                                                    => false
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
    ProcApply.applyProc(proc, prefixArgs ++ tailList)
