package ming

import scala.annotation.tailrec

private[ming] object Level1ValueSupport:

  def eqValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (IntValue(leftNumber), IntValue(rightNumber)) => leftNumber == rightNumber
      case (BoolValue(leftBool), BoolValue(rightBool))   => leftBool == rightBool
      case (leftString: StringLikeValue, rightString: StringLikeValue) =>
        leftString.text == rightString.text
      case (CharValue(leftChar), CharValue(rightChar))         => leftChar == rightChar
      case (SymbolValue(leftSymbol), SymbolValue(rightSymbol)) => leftSymbol == rightSymbol
      case (EmptyListValue, EmptyListValue)                    => true
      case _                                                   => left eq right

  def equalValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (PairValue(leftCar, leftCdr), PairValue(rightCar, rightCdr)) =>
        equalValues(leftCar, rightCar) && equalValues(leftCdr, rightCdr)
      case _ =>
        eqValues(left, right)

  def isString(value: Value): Boolean =
    value match
      case _: StringLikeValue => true
      case _                  => false

  def isNumber(value: Value): Boolean =
    value match
      case IntValue(_) => true
      case _           => false

  def isBoolean(value: Value): Boolean =
    value match
      case BoolValue(_) => true
      case _            => false

  def isPair(value: Value): Boolean =
    value match
      case PairValue(_, _) => true
      case _               => false

  def isSymbol(value: Value): Boolean =
    value match
      case SymbolValue(_) => true
      case _              => false

  def isChar(value: Value): Boolean =
    value match
      case CharValue(_) => true
      case _            => false

  @tailrec
  def isProperList(value: Value): Boolean =
    value match
      case EmptyListValue   => true
      case PairValue(_, xs) => isProperList(xs)
      case _                => false
