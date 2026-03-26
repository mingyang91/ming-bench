package ming

import scala.collection.mutable

private[ming] object SchemeModel:

  enum Expr:
    case IntegerLiteral(value: BigInt)
    case BooleanLiteral(value: Boolean)
    case StringLiteral(value: String)
    case Symbol(name: String)
    case ListExpr(items: List[Expr])

  enum Value:
    case IntegerValue(value: BigInt)
    case BooleanValue(value: Boolean)
    case StringValue(value: String)
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
