package ming

import scala.collection.mutable

class MutablePair(var car: SchemeVal, var cdr: SchemeVal)

class WindEntry(val inThunk: SchemeVal, val outThunk: SchemeVal)

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
  case PairVal(pair: MutablePair)
  case Procedure(params: List[String], restParam: Option[String], body: List[Expr], env: Env)
  case BuiltinProc(name: String, fn: List[SchemeVal] => SchemeVal)
  case Macro(literals: Set[String], rules: List[(Expr, Expr)], defEnv: Env)
  case CaseLambda(clauses: List[(List[String], Option[String], List[Expr], Env)])
  case ContinuationVal(id: Long, body: List[Expr], startIdx: Int, bodyEnv: Env, savedWind: List[WindEntry])
  case VectorVal(elems: Array[SchemeVal])
  case RecordVal(typeName: String, fields: Map[String, SchemeVal])
  case TailCall(expr: Expr, env: Env)
  case ValuesVal(vals: List[SchemeVal])
  case Void

  def display: String = this match
    case IntVal(n) => n.toString
    case FloatVal(d) =>
      if d == d.toLong.toDouble && !d.isInfinite then s"${d.toLong}.0"
      else d.toString
    case RatVal(n, d)                   => s"$n/$d"
    case BoolVal(b)                     => if b then "#t" else "#f"
    case StrVal(s)                      => "\"" + new String(s) + "\""
    case SymVal(n)                      => n
    case CharVal(c)                     => s"#\\$c"
    case ListVal(Nil)                   => "()"
    case ListVal(elems)                 => "(" + elems.map(_.display).mkString(" ") + ")"
    case PairVal(_)                     => formatPair(_.display)
    case Procedure(_, _, _, _)          => "#<procedure>"
    case CaseLambda(_)                  => "#<procedure>"
    case ContinuationVal(_, _, _, _, _) => "#<continuation>"
    case BuiltinProc(name, _)           => s"#<procedure:$name>"
    case Macro(_, _, _)                 => "#<macro>"
    case VectorVal(elems)               => "#(" + elems.map(_.display).mkString(" ") + ")"
    case RecordVal(t, _)                => s"#<$t>"
    case TailCall(_, _)                 => "#<tail-call>"
    case ValuesVal(vals)                => vals.map(_.display).mkString(" ")
    case Void                           => "#<void>"

  /** display format: no quotes on strings */
  def displayStr: String = this match
    case StrVal(s)        => new String(s)
    case ListVal(Nil)     => "()"
    case ListVal(elems)   => "(" + elems.map(_.displayStr).mkString(" ") + ")"
    case VectorVal(elems) => "#(" + elems.map(_.displayStr).mkString(" ") + ")"
    case PairVal(_)       => formatPair(_.displayStr)
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
    val seen = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[MutablePair, java.lang.Boolean]()
    )
    while cur.isInstanceOf[PairVal] do
      val PairVal(p) = cur: @unchecked
      if seen.contains(p) then
        sb.append(" ...")
        return sb.append(")").toString
      seen.add(p)
      if !first then sb.append(" ")
      first = false
      sb.append(fmt(p.car))
      cur = p.cdr
    cur match
      case ListVal(Nil) => ()
      case ListVal(elems) =>
        for e <- elems do sb.append(" ").append(fmt(e))
      case other =>
        sb.append(" . ").append(fmt(other))
    sb.append(")")
    sb.toString

object SchemeVal:

  /** Build a proper list (PairVal chain terminated by ListVal(Nil)) from a Scala list */
  def schemeList(elems: List[SchemeVal]): SchemeVal =
    elems.foldRight(ListVal(Nil): SchemeVal)((e, acc) => PairVal(new MutablePair(e, acc)))

  /** Extract a Scala List from a proper Scheme list (PairVal chain or ListVal) */
  def toScalaList(v: SchemeVal): List[SchemeVal] =
    val buf = List.newBuilder[SchemeVal]
    var cur = v
    while true do
      cur match
        case PairVal(p)     => buf += p.car; cur = p.cdr
        case ListVal(Nil)   => return buf.result()
        case ListVal(elems) => return buf.result() ++ elems
        case _              => throw new EvalError(s"not a proper list")
    buf.result()

  /** Check if a value is a proper list (with cycle detection via tortoise-and-hare) */
  def isProperList(v: SchemeVal): Boolean =
    var slow = v
    var fast = v
    while true do
      // fast step 1
      fast match
        case PairVal(p) => fast = p.cdr
        case ListVal(_) => return true
        case _          => return false
      // fast step 2
      fast match
        case PairVal(p) => fast = p.cdr
        case ListVal(_) => return true
        case _          => return false
      // slow step 1
      slow match
        case PairVal(p) => slow = p.cdr
        case _          => return true
      // cycle check — compare MutablePair identity
      (slow, fast) match
        case (PairVal(s), PairVal(f)) if s eq f => return false
        case _                                  => ()
    false

  /** eq? semantics: reference equality for most types, value equality for immediates */
  def schemeEq(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SymVal(x), SymVal(y))       => x == y
    case (IntVal(x), IntVal(y))       => x == y
    case (BoolVal(x), BoolVal(y))     => x == y
    case (CharVal(x), CharVal(y))     => x == y
    case (ListVal(Nil), ListVal(Nil)) => true
    case _                            => a eq b

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

case class BodyContext(body: List[Expr], idx: Int, env: Env)

class ContinuationReturn(
  val contId: Long,
  val value: SchemeVal,
  val body: List[Expr],
  val startIdx: Int,
  val bodyEnv: Env,
  val callerWind: List[WindEntry] = Nil
) extends Throwable(null, null, true, false):
  override def fillInStackTrace(): Throwable = this
