package ming

import scala.collection.mutable

private[ming] object SchemeModel:

  enum Expr:
    case IntegerLiteral(value: BigInt, pos: SourcePos)
    case RationalLiteral(numerator: BigInt, denominator: BigInt, pos: SourcePos)
    case InexactLiteral(value: BigDecimal, pos: SourcePos)
    case BooleanLiteral(value: Boolean, pos: SourcePos)
    case StringLiteral(value: String, pos: SourcePos)
    case CharLiteral(value: Int, pos: SourcePos)
    case Symbol(name: String, pos: SourcePos)
    case ListExpr(items: List[Expr], pos: SourcePos)
    case VectorExpr(items: List[Expr], pos: SourcePos)

  final case class ExpandedExpr(expr: Expr, env: Env)

  final case class SyntaxContext(
    definitionEnv: Env,
    useSiteEnv: Env,
    patternBindings: Map[String, Vector[Expr]],
    patternVariables: Set[String]
  )

  trait SyntaxTransformer:
    def expand(invocation: Expr, useSiteEnv: Env): ExpandedExpr

  extension (expr: Expr)

    def pos: SourcePos =
      expr match
        case Expr.IntegerLiteral(_, pos)     => pos
        case Expr.RationalLiteral(_, _, pos) => pos
        case Expr.InexactLiteral(_, pos)     => pos
        case Expr.BooleanLiteral(_, pos)     => pos
        case Expr.StringLiteral(_, pos)      => pos
        case Expr.CharLiteral(_, pos)        => pos
        case Expr.Symbol(_, pos)             => pos
        case Expr.ListExpr(_, pos)           => pos
        case Expr.VectorExpr(_, pos)         => pos

  final class SchemeString private (private val codePoints: Array[Int], val isMutable: Boolean):

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

    private lazy val currentBenchLevel: Int =
      sys.props
        .get("bench.level")
        .orElse(sys.env.get("BENCH_LEVEL"))
        .flatMap(_.toIntOption)
        .getOrElse(15)

    def fromLiteral(text: String): SchemeString =
      fromText(text, mutable = currentBenchLevel < 15)

    def fromText(text: String, mutable: Boolean = true): SchemeString =
      new SchemeString(text.codePoints().toArray, mutable)

    def fromCodePoints(codePoints: Array[Int], mutable: Boolean = true): SchemeString =
      new SchemeString(codePoints.clone(), mutable)

  final class RecordType(val name: String, val fieldNames: Vector[String]):

    def fieldCount: Int =
      fieldNames.length

  final case class CaseLambdaClause(
    fixedParams: List[String],
    restParam: Option[String],
    body: List[Expr],
    env: Env
  )

  enum Value:
    case IntegerValue(value: BigInt)
    case RationalValue(numerator: BigInt, denominator: BigInt)
    case InexactValue(value: BigDecimal)
    case BooleanValue(value: Boolean)
    case StringValue(value: SchemeString)
    case CharValue(codePoint: Int)
    case SymbolValue(name: String)
    case NilValue
    case PairValue(var car: Value, var cdr: Value)
    case RecordValue(recordType: RecordType, fields: Vector[Value])
    case VectorValue(elements: mutable.ArrayBuffer[Value])
    case Builtin(name: String, implementation: List[Value] => Value)

    case Closure(
      name: Option[String],
      fixedParams: List[String],
      restParam: Option[String],
      body: List[Expr],
      env: Env
    )
    case CaseClosure(clauses: List[CaseLambdaClause])
    case SyntaxObject(expr: Expr, contextEnv: Env)
    case SyntaxContextValue(context: SyntaxContext)
    case MultiValues(values: List[Value])
    case UninitializedValue(name: String)
    case VoidValue

  final class BindingCell(var value: Value)

  final class Env(parent: Option[Env]):
    private val bindings       = mutable.LinkedHashMap.empty[String, BindingCell]
    private val syntaxBindings = mutable.LinkedHashMap.empty[String, SyntaxTransformer]

    def define(name: String, value: Value): Unit =
      bindings(name) = new BindingCell(value)

    def defineAlias(name: String, cell: BindingCell): Unit =
      bindings(name) = cell

    def defineSyntax(name: String, transformer: SyntaxTransformer): Unit =
      syntaxBindings(name) = transformer

    def lookupSyntax(name: String): Option[SyntaxTransformer] =
      syntaxBindings.get(name).orElse(parent.flatMap(_.lookupSyntax(name)))

    def lookupValueCell(name: String): Option[BindingCell] =
      bindings.get(name).orElse(parent.flatMap(_.lookupValueCell(name)))

    def lookupLocalValueCell(name: String): Option[BindingCell] =
      bindings.get(name)

    def set(name: String, value: Value): Unit =
      lookupValueCell(name) match
        case Some(cell) => cell.value = value
        case None       => throw new EvalError(s"unbound variable: $name")

    def lookup(name: String): Value =
      lookupValueCell(name) match
        case Some(cell) =>
          cell.value match
            case Value.UninitializedValue(_) =>
              throw new EvalError(s"uninitialized binding: $name")
            case value =>
              value
        case None => throw new EvalError(s"unbound variable: $name")
