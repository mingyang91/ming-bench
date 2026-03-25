package ming

import scala.annotation.tailrec

private[ming] enum Value:

  case IntVal(value: BigInt)
  case RationalVal(numerator: BigInt, denominator: BigInt)
  case InexactVal(value: Double)
  case BoolVal(value: Boolean)
  case StringVal(value: Array[Char])
  case CharVal(value: Char)
  case SymbolVal(name: String)
  case EmptyList
  case PairVal(car: Value, cdr: Value)
  case RecordVal(recordType: RecordType, fields: Array[Value])
  case Closure(params: List[String], restParam: Option[String], body: List[Expr], env: Env)
  case CaseClosure(clauses: List[ProcedureClause], env: Env)
  case Builtin(name: String, fn: (List[Value], SourcePos) => Value)
  case VoidVal

  def render: String =
    Value.renderValue(this, displayMode = false)

  def renderDisplay: String =
    Value.renderValue(this, displayMode = true)

  def typeName: String =
    this match
      case Value.IntVal(_) | Value.RationalVal(_, _) | Value.InexactVal(_) =>
        "number"

      case Value.BoolVal(_) =>
        "boolean"

      case Value.StringVal(_) =>
        "string"

      case Value.CharVal(_) =>
        "char"

      case Value.SymbolVal(_) =>
        "symbol"

      case Value.EmptyList =>
        "null"

      case Value.PairVal(_, _) =>
        "pair"

      case Value.RecordVal(_, _) =>
        "record"

      case Value.Closure(_, _, _, _) | Value.CaseClosure(_, _) | Value.Builtin(_, _) =>
        "procedure"

      case Value.VoidVal =>
        "void"

private[ming] object Value:

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BoolVal(false) =>
        false

      case _ =>
        true

  def isCallable(value: Value): Boolean =
    value match
      case Value.Closure(_, _, _, _) | Value.CaseClosure(_, _) | Value.Builtin(_, _) =>
        true

      case _ =>
        false

  def isProperList(value: Value): Boolean =
    @tailrec
    def loop(current: Value): Boolean =
      current match
        case Value.EmptyList =>
          true

        case Value.PairVal(_, cdr) =>
          loop(cdr)

        case _ =>
          false

    loop(value)

  def eqv(left: Value, right: Value): Boolean =
    (left, right) match
      case _ if isNumberValue(left) && isNumberValue(right) =>
        equalNumbers(left, right)

      case (Value.BoolVal(leftValue), Value.BoolVal(rightValue)) =>
        leftValue == rightValue

      case (Value.CharVal(leftValue), Value.CharVal(rightValue)) =>
        leftValue == rightValue

      case (Value.SymbolVal(leftValue), Value.SymbolVal(rightValue)) =>
        leftValue == rightValue

      case (Value.EmptyList, Value.EmptyList) =>
        true

      case (Value.StringVal(leftValue), Value.StringVal(rightValue)) =>
        leftValue eq rightValue

      case _ =>
        left eq right

  def equal(left: Value, right: Value): Boolean =
    (left, right) match
      case (Value.StringVal(leftValue), Value.StringVal(rightValue)) =>
        leftValue.sameElements(rightValue)

      case (Value.PairVal(leftCar, leftCdr), Value.PairVal(rightCar, rightCdr)) =>
        equal(leftCar, rightCar) && equal(leftCdr, rightCdr)

      case _ =>
        eqv(left, right)

  def escapeString(value: String): String =
    val builder = new StringBuilder
    value.foreach {
      case '\\' =>
        builder.append("\\\\")

      case '"' =>
        builder.append("\\\"")

      case '\n' =>
        builder.append("\\n")

      case '\r' =>
        builder.append("\\r")

      case '\t' =>
        builder.append("\\t")

      case ch =>
        builder.append(ch)
    }
    builder.toString

  def renderValue(value: Value, displayMode: Boolean): String =
    value match
      case Value.IntVal(number) =>
        number.toString

      case Value.RationalVal(numerator, denominator) =>
        s"$numerator/$denominator"

      case Value.InexactVal(number) =>
        java.lang.Double.toString(number)

      case Value.BoolVal(boolean) =>
        if boolean then "#t" else "#f"

      case Value.StringVal(chars) =>
        val text = new String(chars)
        if displayMode then text else "\"" + escapeString(text) + "\""

      case Value.CharVal(ch) =>
        if displayMode then ch.toString else renderChar(ch)

      case Value.SymbolVal(name) =>
        name

      case Value.EmptyList =>
        "()"

      case Value.PairVal(_, _) =>
        renderPair(value, displayMode)

      case Value.RecordVal(recordType, _) =>
        s"#<record:${recordType.displayName}>"

      case Value.Closure(_, _, _, _) | Value.CaseClosure(_, _) =>
        "#<procedure>"

      case Value.Builtin(name, _) =>
        s"#<procedure:$name>"

      case Value.VoidVal =>
        "#<void>"

  def fromQuotedExpr(expr: Expr): Value =
    expr match
      case Expr.IntAtom(value, _) =>
        Value.IntVal(value)

      case Expr.RationalAtom(numerator, denominator, _) =>
        Value.RationalVal(numerator, denominator)

      case Expr.InexactAtom(value, _) =>
        Value.InexactVal(value)

      case Expr.BoolAtom(value, _) =>
        Value.BoolVal(value)

      case Expr.StringAtom(value, _) =>
        Value.StringVal(value.toCharArray)

      case Expr.CharAtom(value, _) =>
        Value.CharVal(value)

      case Expr.Symbol(name, _) =>
        Value.SymbolVal(name)

      case Expr.ListExpr(items, _) =>
        Value.list(items.map(fromQuotedExpr))

  def list(items: List[Value]): Value =
    items.foldRight[Value](Value.EmptyList)(Value.PairVal(_, _))

  private def isNumberValue(value: Value): Boolean =
    SchemeNumber.isNumberValue(value)

  private def equalNumbers(left: Value, right: Value): Boolean =
    (SchemeNumber.fromValueOption(left), SchemeNumber.fromValueOption(right)) match
      case (Some(leftNumber), Some(rightNumber)) =>
        SchemeNumber.equal(leftNumber, rightNumber)

      case _ =>
        false

  private def renderChar(value: Char): String =
    value match
      case ' ' =>
        "#\\space"

      case '\n' =>
        "#\\newline"

      case other =>
        s"#\\$other"

  private def renderPair(value: Value, displayMode: Boolean): String =
    val builder = new StringBuilder
    builder.append('(')
    appendPairContents(value, builder, displayMode)
    builder.append(')')
    builder.toString

  @tailrec
  private def appendPairContents(
    value: Value,
    builder: StringBuilder,
    displayMode: Boolean
  ): Unit =
    value match
      case Value.PairVal(car, cdr) =>
        builder.append(renderValue(car, displayMode))
        cdr match
          case Value.EmptyList =>
            ()

          case next @ Value.PairVal(_, _) =>
            builder.append(' ')
            appendPairContents(next, builder, displayMode)

          case other =>
            builder.append(" . ")
            builder.append(renderValue(other, displayMode))

      case other =>
        builder.append(renderValue(other, displayMode))
