package ming

import scala.collection.mutable

private[ming] trait SchemeInterpreterTypes:

  sealed trait Expr:
    def pos: SourcePos

  object Expr:
    final case class Number(value: SchemeNumber, pos: SourcePos) extends Expr
    final case class Bool(value: Boolean, pos: SourcePos)        extends Expr
    final case class StringLit(value: String, pos: SourcePos)    extends Expr
    final case class Character(value: Char, pos: SourcePos)      extends Expr
    final case class Symbol(name: String, pos: SourcePos)        extends Expr
    final case class ListExpr(items: List[Expr], pos: SourcePos) extends Expr

  sealed trait Value
  sealed trait Procedure extends Value

  object Value:
    final case class Number(value: SchemeNumber) extends Value
    final case class Bool(value: Boolean)        extends Value
    final case class StringLit(value: String)    extends Value

    final class MutableString private (private val chars: mutable.ArrayBuffer[Char]) extends Value:

      def value: String =
        chars.mkString

      def length: Int =
        chars.length

      def charAt(index: Int): Char =
        chars(index)

      def set(index: Int, value: Char): Unit =
        chars(index) = value

      def copyString(): MutableString =
        MutableString(value)

    object MutableString:

      def apply(value: String): MutableString =
        new MutableString(mutable.ArrayBuffer.from(value))

      def unapply(value: MutableString): Some[String] =
        Some(value.value)

    final case class Character(value: Char)       extends Value
    final case class Symbol(name: String)         extends Value
    case object EmptyList                         extends Value
    final case class Pair(car: Value, cdr: Value) extends Value

    final class Vector private (private val elements: mutable.ArrayBuffer[Value]) extends Value:

      def length: Int =
        elements.length

      def elementAt(index: Int): Value =
        elements(index)

      def set(index: Int, value: Value): Unit =
        elements(index) = value

      def toList: List[Value] =
        elements.toList

    object Vector:

      def apply(values: Iterable[Value]): Vector =
        new Vector(mutable.ArrayBuffer.from(values))

    final class Record private[ming] (
      private val descriptor: SchemeRecords.RecordTypeDescriptor,
      private val fields: mutable.ArrayBuffer[Value]
    ) extends Value:

      def recordTypeId: Long =
        descriptor.id

      def typeName: String =
        descriptor.name

      def field(index: Int): Value =
        fields(index)

      def setField(index: Int, value: Value): Unit =
        fields(index) = value

    final case class Builtin(name: String, impl: (List[Value], SourcePos) => Value) extends Procedure

    final case class Closure(
      params: LambdaParams,
      body: List[Expr],
      env: Env,
      macros: MacroScope
    ) extends Procedure

    final case class CaseLambdaClause(
      params: LambdaParams,
      body: List[Expr]
    )

    final case class CaseLambda(
      clauses: List[CaseLambdaClause],
      env: Env,
      macros: MacroScope
    ) extends Procedure

    case object Void extends Value

    def list(items: List[Value]): Value =
      items.foldRight[Value](EmptyList)(Pair(_, _))
