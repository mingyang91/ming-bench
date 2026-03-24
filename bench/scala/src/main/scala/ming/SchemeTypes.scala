package ming

import scala.collection.mutable

enum Expr:
  case IntLit(value: Long)
  case FloatLit(value: Double)
  case RatLit(num: Long, den: Long)
  case BoolLit(value: Boolean)
  case StrLit(value: String)
  case Symbol(name: String)
  case CharLit(value: Char)
  case SList(elems: List[Expr])

enum SchemeVal:
  case IntVal(value: Long)
  case FloatVal(value: Double)
  case RatVal(num: Long, den: Long)
  case BoolVal(value: Boolean)
  case StrVal(value: Array[Char])
  case SymVal(name: String)
  case CharVal(value: Char)
  case ListVal(elems: List[SchemeVal])
  case PairVal(car: SchemeVal, cdr: SchemeVal)
  case Procedure(params: List[String], restParam: Option[String], body: List[Expr], env: Env)
  case BuiltinProc(name: String, fn: List[SchemeVal] => SchemeVal)
  case Macro(literals: Set[String], rules: List[(Expr, Expr)], defEnv: Env)
  case CaseLambda(clauses: List[(List[String], Option[String], List[Expr], Env)])
  case VectorVal(elems: Array[SchemeVal])
  case RecordVal(typeName: String, fields: Map[String, SchemeVal])
  case TailCall(expr: Expr, env: Env)
  case Void

  def display: String = this match
    case IntVal(n) => n.toString
    case FloatVal(d) =>
      if d == d.toLong.toDouble && !d.isInfinite then s"${d.toLong}.0"
      else d.toString
    case RatVal(n, d)          => s"$n/$d"
    case BoolVal(b)            => if b then "#t" else "#f"
    case StrVal(s)             => "\"" + new String(s) + "\""
    case SymVal(n)             => n
    case CharVal(c)            => s"#\\$c"
    case ListVal(Nil)          => "()"
    case ListVal(elems)        => "(" + elems.map(_.display).mkString(" ") + ")"
    case PairVal(_, _)         => formatPair(_.display)
    case Procedure(_, _, _, _) => "#<procedure>"
    case CaseLambda(_)         => "#<procedure>"
    case BuiltinProc(name, _)  => s"#<procedure:$name>"
    case Macro(_, _, _)        => "#<macro>"
    case VectorVal(elems)      => "#(" + elems.map(_.display).mkString(" ") + ")"
    case RecordVal(t, _)       => s"#<$t>"
    case TailCall(_, _)        => "#<tail-call>"
    case Void                  => "#<void>"

  /** display format: no quotes on strings */
  def displayStr: String = this match
    case StrVal(s)        => new String(s)
    case ListVal(Nil)     => "()"
    case ListVal(elems)   => "(" + elems.map(_.displayStr).mkString(" ") + ")"
    case VectorVal(elems) => "#(" + elems.map(_.displayStr).mkString(" ") + ")"
    case PairVal(_, _)    => formatPair(_.displayStr)
    case other            => other.display

  def isNumber: Boolean = this match
    case IntVal(_) | FloatVal(_) | RatVal(_, _) => true
    case _                                      => false

  /** write format: strings with quotes */
  def writeStr: String = this match
    case StrVal(s) => "\"" + new String(s) + "\""
    case _         => displayStr

  private def formatPair(fmt: SchemeVal => String): String =
    val sb             = new StringBuilder("(")
    var cur: SchemeVal = this
    var first          = true
    while cur.isInstanceOf[PairVal] do
      if !first then sb.append(" ")
      first = false
      val PairVal(h, t) = cur: @unchecked
      sb.append(fmt(h))
      cur = t
    cur match
      case ListVal(Nil) => ()
      case ListVal(elems) =>
        for e <- elems do sb.append(" ").append(fmt(e))
      case other =>
        sb.append(" . ").append(fmt(other))
    sb.append(")")
    sb.toString

class Env(
  val bindings: mutable.Map[String, SchemeVal],
  val parent: Option[Env]
):

  def lookup(name: String): SchemeVal =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def lookupOpt(name: String): Option[SchemeVal] =
    bindings.get(name).orElse(parent.flatMap(_.lookupOpt(name)))

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

  def set(name: String, value: SchemeVal): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value)
        case None    => throw new EvalError(s"set!: unbound variable: $name")
