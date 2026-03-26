package ming

import scala.collection.mutable

private[ming] object SchemeModel:

  enum Expr:
    case IntegerLiteral(value: BigInt, pos: SourcePos)
    case BooleanLiteral(value: Boolean, pos: SourcePos)
    case StringLiteral(value: String, pos: SourcePos)
    case CharLiteral(value: Int, pos: SourcePos)
    case Symbol(name: String, pos: SourcePos)
    case ListExpr(items: List[Expr], pos: SourcePos)

  extension (expr: Expr)

    def pos: SourcePos =
      expr match
        case Expr.IntegerLiteral(_, pos) => pos
        case Expr.BooleanLiteral(_, pos) => pos
        case Expr.StringLiteral(_, pos)  => pos
        case Expr.CharLiteral(_, pos)    => pos
        case Expr.Symbol(_, pos)         => pos
        case Expr.ListExpr(_, pos)       => pos

  final class SchemeString private (private val codePoints: Array[Int]):

    def text: String =
      new String(codePoints, 0, codePoints.length)

    def length: Int =
      codePoints.length

    def copyString(): SchemeString =
      SchemeString.fromCodePoints(codePoints)

    def slice(start: Int, end: Int): SchemeString =
      SchemeString.fromCodePoints(codePoints.slice(start, end))

    def codePointAt(index: Int): Int =
      codePoints(index)

    def setCodePoint(index: Int, value: Int): Unit =
      codePoints(index) = value

    def codePointsArray: Array[Int] =
      codePoints.clone()

  object SchemeString:

    def fromText(text: String): SchemeString =
      new SchemeString(text.codePoints().toArray)

    def fromCodePoints(codePoints: Array[Int]): SchemeString =
      new SchemeString(codePoints.clone())

  enum Value:
    case IntegerValue(value: BigInt)
    case BooleanValue(value: Boolean)
    case StringValue(value: SchemeString)
    case CharValue(codePoint: Int)
    case SymbolValue(name: String)
    case NilValue
    case PairValue(car: Value, cdr: Value)
    case Builtin(name: String, implementation: List[Value] => Value)
    case Closure(name: Option[String], params: List[String], body: List[Expr], env: Env)
    case VoidValue

  final class Env(parent: Option[Env]):
    private val bindings = mutable.LinkedHashMap.empty[String, Value]

    def define(name: String, value: Value): Unit =
      bindings(name) = value

    def lookup(name: String): Value =
      bindings.get(name) match
        case Some(value) => value
        case None =>
          parent match
            case Some(outer) => outer.lookup(name)
            case None        => throw new EvalError(s"unbound variable: $name")
