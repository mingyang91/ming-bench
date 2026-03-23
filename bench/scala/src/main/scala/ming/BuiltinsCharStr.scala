package ming

import SchemeValue.*

/** Char and string comparison/case operations. */
object BuiltinsCharStr:

  private def extractString(v: SchemeValue): String = v match
    case StringVal(s, _)         => s
    case MutableStringVal(cs, _) => String(cs)
    case _                       => throw new EvalError("expected string")

  def charToIntegerOp(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => IntVal(c.toLong)
      case _                    => throw new EvalError("char->integer: expected char")

  def integerToCharOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => CharVal(n.toChar)
      case _                   => throw new EvalError("integer->char: expected integer")

  def charAlphabeticCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => BoolVal(c.isLetter)
      case _                    => throw new EvalError("char-alphabetic?: expected 1 char")

  def charNumericCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => BoolVal(c.isDigit)
      case _                    => throw new EvalError("char-numeric?: expected 1 char")

  def charUpcaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => CharVal(c.toUpper)
      case _                    => throw new EvalError("char-upcase: expected 1 char")

  def charDowncaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => CharVal(c.toLower)
      case _                    => throw new EvalError("char-downcase: expected 1 char")

  def charEqualCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(a, _) :: CharVal(b, _) :: Nil => BoolVal(a == b)
      case _                                     => throw new EvalError("char=?: expected 2 chars")

  def charLessCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(a, _) :: CharVal(b, _) :: Nil => BoolVal(a < b)
      case _                                     => throw new EvalError("char<?: expected 2 chars")

  def stringEqualCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) == extractString(b))
      case _             => throw new EvalError("string=?: expected 2 strings")

  def stringLessCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) < extractString(b))
      case _             => throw new EvalError("string<?: expected 2 strings")

  def stringGreaterCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) > extractString(b))
      case _             => throw new EvalError("string>?: expected 2 strings")

  def stringLessEqCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) <= extractString(b))
      case _             => throw new EvalError("string<=?: expected 2 strings")

  def stringGreaterEqCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) >= extractString(b))
      case _             => throw new EvalError("string>=?: expected 2 strings")

  def stringCiEqualCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a).equalsIgnoreCase(extractString(b)))
      case _             => throw new EvalError("string-ci=?: expected 2 strings")

  def stringUpcaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => StringVal(extractString(v).toUpperCase)
      case _        => throw new EvalError("string-upcase: expected 1 string")

  def stringDowncaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => StringVal(extractString(v).toLowerCase)
      case _        => throw new EvalError("string-downcase: expected 1 string")
