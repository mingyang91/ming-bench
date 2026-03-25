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

  sealed private[ming] trait EvalState

  private[ming] type Resume = Value => EvalState

  final class DynamicWindFrame private[ming] (
    val before: Value,
    val after: Value,
    val pos: SourcePos
  )

  object DynamicWindFrame:

    def apply(before: Value, after: Value, pos: SourcePos): DynamicWindFrame =
      new DynamicWindFrame(before, after, pos)

  final class ExceptionHandlerFrame private[ming] (
    val handler: Value,
    val windStack: scala.collection.immutable.Vector[DynamicWindFrame],
    val resume: Resume
  )

  object ExceptionHandlerFrame:

    def apply(
      handler: Value,
      windStack: scala.collection.immutable.Vector[DynamicWindFrame],
      resume: Resume
    ): ExceptionHandlerFrame =
      new ExceptionHandlerFrame(handler, windStack, resume)

  object EvalState:

    final case class EvaluateExpr(
      expr: Expr,
      env: Env,
      macros: MacroScope,
      cont: Resume
    ) extends EvalState

    final case class EvaluateSequence(
      expressions: List[Expr],
      env: Env,
      macros: MacroScope,
      cont: Resume
    ) extends EvalState

    final case class PopExceptionHandler(
      frame: ExceptionHandlerFrame,
      value: Value,
      cont: Resume
    ) extends EvalState

    final case class Done(value: Value) extends EvalState

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

    final case class Character(value: Char) extends Value
    final case class Symbol(name: String)   extends Value
    case object EmptyList                   extends Value

    final class Pair private (private var currentCar: Value, private var currentCdr: Value) extends Value:

      def car: Value =
        currentCar

      def cdr: Value =
        currentCdr

      def setCar(value: Value): Unit =
        currentCar = value

      def setCdr(value: Value): Unit =
        currentCdr = value

    object Pair:

      def apply(car: Value, cdr: Value): Pair =
        new Pair(car, cdr)

      def unapply(pair: Pair): Some[(Value, Value)] =
        Some((pair.car, pair.cdr))

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
    case object ApplyProcedureBuiltin                                               extends Procedure
    case object MapProcedureBuiltin                                                 extends Procedure
    case object ForEachProcedureBuiltin                                             extends Procedure
    case object ValuesBuiltin                                                       extends Procedure
    case object CallWithValuesBuiltin                                               extends Procedure
    case object CallWithCurrentContinuation                                         extends Procedure
    case object DynamicWindBuiltin                                                  extends Procedure
    case object RaiseBuiltin                                                        extends Procedure
    case object WithExceptionHandlerBuiltin                                         extends Procedure

    final class Continuation private[ming] (
      val resume: Resume,
      val windStack: scala.collection.immutable.Vector[DynamicWindFrame],
      val handlerStack: scala.collection.immutable.Vector[ExceptionHandlerFrame]
    ) extends Procedure

    object Continuation:

      def apply(
        resume: Resume,
        windStack: scala.collection.immutable.Vector[DynamicWindFrame],
        handlerStack: scala.collection.immutable.Vector[ExceptionHandlerFrame]
      ): Continuation =
        new Continuation(resume, windStack, handlerStack)

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

    final case class MultipleValues(values: List[Value]) extends Value

    final case class SyntaxObject(expr: Expr) extends Value

    case object Void extends Value

    def list(items: List[Value]): Value =
      items.foldRight[Value](EmptyList)(Pair(_, _))
