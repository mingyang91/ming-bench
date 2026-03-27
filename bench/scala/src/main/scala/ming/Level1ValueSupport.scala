package ming

import scala.annotation.tailrec

private[ming] object Level1ValueSupport:

  def eqvValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (leftNumber: NumberValue, rightNumber: NumberValue) =>
        NumericSupport.equal(leftNumber, rightNumber)
      case (BoolValue(leftBool), BoolValue(rightBool)) => leftBool == rightBool
      case (leftString: StringLikeValue, rightString: StringLikeValue) =>
        leftString.text == rightString.text
      case (CharValue(leftChar), CharValue(rightChar))         => leftChar == rightChar
      case (SymbolValue(leftSymbol), SymbolValue(rightSymbol)) => leftSymbol == rightSymbol
      case (EmptyListValue, EmptyListValue)                    => true
      case _                                                   => left eq right

  def eqValues(left: Value, right: Value): Boolean =
    eqvValues(left, right)

  def equalValues(left: Value, right: Value): Boolean =
    (left, right) match
      case (PairValue(leftCar, leftCdr), PairValue(rightCar, rightCdr)) =>
        equalValues(leftCar, rightCar) && equalValues(leftCdr, rightCdr)
      case (leftVector: VectorValue, rightVector: VectorValue) =>
        leftVector.length == rightVector.length &&
        leftVector.toList
          .zip(rightVector.toList)
          .forall((leftElement, rightElement) => equalValues(leftElement, rightElement))
      case _ =>
        eqvValues(left, right)

  def isString(value: Value): Boolean =
    value match
      case _: StringLikeValue => true
      case _                  => false

  def isNumber(value: Value): Boolean =
    value match
      case _: NumberValue => true
      case _              => false

  def isExactNumber(value: Value): Boolean =
    value match
      case number: NumberValue => NumericSupport.isExact(number)
      case _                   => false

  def isInexactNumber(value: Value): Boolean =
    value match
      case number: NumberValue => NumericSupport.isInexact(number)
      case _                   => false

  def isIntegerNumber(value: Value): Boolean =
    value match
      case number: NumberValue => NumericSupport.isInteger(number)
      case _                   => false

  def isRationalNumber(value: Value): Boolean =
    value match
      case number: NumberValue => NumericSupport.isRational(number)
      case _                   => false

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

  def isVector(value: Value): Boolean =
    value match
      case _: VectorValue => true
      case _              => false

  @tailrec
  def isProperList(value: Value): Boolean =
    value match
      case EmptyListValue   => true
      case PairValue(_, xs) => isProperList(xs)
      case _                => false
