package ming

case class Pos(line: Int, col: Int)

/** Mutable cons cell for pair mutation (set-car!/set-cdr!). */
class MutableCell(var car: SchemeVal, var cdr: SchemeVal)

/** Scheme value types */
enum SchemeVal:
  var pos: Option[Pos] = None
  case SInt(value: Long)
  case SFloat(value: Double)
  case SRational(num: Long, den: Long)
  case SBool(value: Boolean)
  case SString(value: StringBuilder, mutable: Boolean)
  case SSymbol(name: String)
  case SChar(value: Char)
  case SList(elems: List[SchemeVal])
  case SPair(cell: MutableCell)
  case SVoid

  case SLambda(
    params: List[String],
    restParam: Option[String],
    body: List[SchemeVal],
    closure: Env
  )

  case SMacro(
    literals: Set[String],
    clauses: List[(SchemeVal, SchemeVal)],
    defEnv: Env
  )

  case SRecord(
    typeName: String,
    fields: Map[String, SchemeVal]
  )

  case SCaseLambda(
    clauses: List[(List[String], Option[String], List[SchemeVal])],
    closure: Env
  )

  case SVector(elems: Array[SchemeVal])

  case SContinuation(k: Cont, winds: List[WindEntry])

  /** Safely cast to SMacro via pattern match. */
  def asMatchedMacro: SchemeVal.SMacro = this match
    case m: SchemeVal.SMacro => m
    case other               => throw new EvalError(s"expected macro, got ${other.display}")

  /** Write representation (with quotes for strings). */
  def display: String = SchemeVal.displayVal(this)

  /** Display representation (no quotes for strings). */
  def displayRepr: String = this match
    case SString(v, _) => v.toString
    case SChar(c)      => c.toString
    case other         => other.display

object SchemeVal:

  private def displayVal(v: SchemeVal): String =
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[MutableCell, java.lang.Boolean]()
    )
    displayImpl(v, visited)

  private def displayImpl(v: SchemeVal, visited: java.util.Set[MutableCell]): String = v match
    case SInt(n) => n.toString
    case SFloat(d) =>
      if d == d.toLong.toDouble && !d.isInfinite then f"$d%.1f"
      else d.toString
    case SRational(n, d) =>
      if d == 1 then n.toString else s"$n/$d"
    case SBool(b)      => if b then "#t" else "#f"
    case SString(s, _) => s""""${s.toString}""""
    case SSymbol(n)    => n
    case SChar(c) =>
      c match
        case ' '  => "#\\space"
        case '\n' => "#\\newline"
        case '\t' => "#\\tab"
        case _    => s"#\\$c"
    case SList(es) => "(" + es.map(displayImpl(_, visited)).mkString(" ") + ")"
    case SPair(cell) =>
      if !visited.add(cell) then "(...)"
      else "(" + pairTailImpl(cell, visited) + ")"
    case SVoid               => ""
    case SLambda(_, _, _, _) => "#<procedure>"
    case SCaseLambda(_, _)   => "#<procedure>"
    case SMacro(_, _, _)     => "#<macro>"
    case SRecord(tn, _)      => s"#<record:$tn>"
    case SVector(es)         => "#(" + es.map(displayImpl(_, visited)).mkString(" ") + ")"
    case SContinuation(_, _) => "#<continuation>"

  private def pairTailImpl(cell: MutableCell, visited: java.util.Set[MutableCell]): String =
    val carStr = displayImpl(cell.car, visited)
    cell.cdr match
      case SList(Nil) => carStr
      case SList(es)  => carStr + " " + es.map(displayImpl(_, visited)).mkString(" ")
      case SPair(c2) =>
        if !visited.add(c2) then carStr + " . (...)"
        else carStr + " " + pairTailImpl(c2, visited)
      case other => carStr + " . " + displayImpl(other, visited)

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a.abs else gcd(b, a % b)

  /** Create a rational, normalizing sign and reducing to lowest terms. Returns SInt if denominator is 1. */
  def makeRational(num: Long, den: Long): SchemeVal =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1L else 1L
    val n    = num * sign
    val d    = den * sign
    val g    = gcd(n.abs, d)
    val rn   = n / g
    val rd   = d / g
    if rd == 1 then SInt(rn) else SRational(rn, rd)

  /** Build a mutable pair chain from a Scala list, terminated by SList(Nil). */
  def buildList(elems: List[SchemeVal]): SchemeVal =
    elems.foldRight(SList(Nil): SchemeVal)((e, acc) => SPair(new MutableCell(e, acc)))

  /** Convert a Scheme list (SList or SPair chain) to a Scala List. Returns None for improper/cyclic lists. */
  def toScalaList(v: SchemeVal): Option[List[SchemeVal]] =
    val buf = scala.collection.mutable.ListBuffer[SchemeVal]()
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[MutableCell, java.lang.Boolean]()
    )
    var cur = v
    while true do
      cur match
        case SList(Nil)   => return Some(buf.toList)
        case SList(elems) => return Some((buf ++= elems).toList)
        case SPair(cell) =>
          if !visited.add(cell) then return None
          buf += cell.car
          cur = cell.cdr
        case _ => return None
    None

  /** Check if a value is a proper list (handles cycles). */
  def isList(v: SchemeVal): Boolean = v match
    case SList(_) => true
    case SPair(_) =>
      // Floyd's tortoise-and-hare cycle detection
      var slow = v
      var fast = v
      while true do
        // Advance fast by 1
        fast match
          case SPair(c) => fast = c.cdr
          case SList(_) => return true
          case _        => return false
        // Check after first step
        fast match
          case SList(_) => return true
          case _        => ()
        // Advance fast by 1 more
        fast match
          case SPair(c) => fast = c.cdr
          case SList(_) => return true
          case _        => return false
        // Check after second step
        fast match
          case SList(_) => return true
          case _        => ()
        // Advance slow by 1
        slow match
          case SPair(c) => slow = c.cdr
          case _        => return true
        // Compare cells for identity
        (slow, fast) match
          case (SPair(s), SPair(f)) if s eq f => return false // cycle
          case _                              => ()
      false
    case _ => false

  /** Helper: get car of a pair-like value. */
  def pairCar(v: SchemeVal): SchemeVal = v match
    case SPair(c)      => c.car
    case SList(h :: _) => h
    case _             => throw new EvalError(s"car: expected pair, got ${v.display}")

  /** Helper: get cdr of a pair-like value. */
  def pairCdr(v: SchemeVal): SchemeVal = v match
    case SPair(c)      => c.cdr
    case SList(_ :: t) => SList(t)
    case _             => throw new EvalError(s"cdr: expected pair, got ${v.display}")

  /** Helper: is value a pair (non-empty list or SPair)? */
  def isPairLike(v: SchemeVal): Boolean = v match
    case SPair(_)      => true
    case SList(_ :: _) => true
    case _             => false
