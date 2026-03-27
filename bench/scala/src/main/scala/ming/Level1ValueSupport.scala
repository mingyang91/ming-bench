package ming

import scala.collection.mutable
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
    equalValues(left, right, mutable.HashSet.empty[SeenPair])

  private def equalValues(
    left: Value,
    right: Value,
    seen: mutable.HashSet[SeenPair]
  ): Boolean =
    if left eq right then true
    else
      (left, right) match
        case (leftPair: PairValue, rightPair: PairValue) =>
          val key = SeenPair(leftPair, rightPair)
          if seen.contains(key) then true
          else
            seen += key
            equalValues(leftPair.car, rightPair.car, seen) &&
            equalValues(leftPair.cdr, rightPair.cdr, seen)
        case (leftVector: VectorValue, rightVector: VectorValue) =>
          if leftVector.length != rightVector.length then false
          else
            val key = SeenPair(leftVector, rightVector)
            if seen.contains(key) then true
            else
              seen += key
              (0 until leftVector.length).forall(index =>
                equalValues(leftVector.element(index), rightVector.element(index), seen)
              )
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

  def isProperList(value: Value): Boolean =
    value match
      case EmptyListValue =>
        true
      case _: PairValue =>
        isProperList(value, value)
      case _ =>
        false

  @tailrec
  private def isProperList(slow: Value, fast: Value): Boolean =
    fast match
      case EmptyListValue =>
        true
      case PairValue(_, fastTail) =>
        fastTail match
          case EmptyListValue =>
            true
          case PairValue(_, fastTailTail) =>
            slow match
              case PairValue(_, slowTail) =>
                if slowTail eq fastTailTail then false
                else isProperList(slowTail, fastTailTail)
              case _ =>
                false
          case _ =>
            false
      case _ =>
        false

  final private case class SeenPair(left: AnyRef, right: AnyRef):

    override def equals(other: Any): Boolean =
      other match
        case that: SeenPair =>
          (left eq that.left) && (right eq that.right)
        case _ =>
          false

    override def hashCode(): Int =
      (31 * System.identityHashCode(left)) + System.identityHashCode(right)
