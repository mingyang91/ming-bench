package ming

object SchemeTypes:

  // ── Mutable pair cell ──────────────────────────────────────────────
  class PairCell(var car: Value, var cdr: Value)

  // ── Values ───────────────────────────────────────────────────────────
  enum Value:
    case VNum(n: Long)
    case VFloat(d: Double)
    case VRational(num: Long, den: Long)
    case VBool(b: Boolean)
    case VStr(chars: Array[Char], mutable: Boolean = true)
    case VChar(c: Char)
    case VList(elems: List[Value])
    case VDottedList(elems: List[Value], last: Value)
    case VPair(cell: PairCell)
    case VSymbol(name: String)
    case VBuiltin(name: String)

    case VLambda(
      params: List[String],
      restParam: Option[String],
      body: List[Expr],
      closure: Env
    )

    case VMacro(
      literals: List[String],
      rules: List[(Expr, Expr)],
      defEnv: Env
    )

    case VCaseLambda(
      clauses: List[(List[String], Option[String], List[Expr], Env)]
    )

    case VVector(elems: Array[Value])

    case VRecord(
      typeName: String,
      fields: Map[String, Value]
    )
    case VContinuation(k: Kont)
    case VVoid

  type Pos = ming.Pos

  def errAt(pos: Pos, msg: String): EvalError =
    EvalError(s"$pos: $msg")

  // ── Environment ──────────────────────────────────────────────────────
  class Env(
    val bindings: scala.collection.mutable.Map[String, Value],
    val parent: Option[Env],
    val output: StringBuilder
  ):

    def lookup(name: String, pos: Pos): Value =
      bindings
        .get(name)
        .orElse(
          parent.flatMap(p => scala.util.Try(p.lookup(name, pos)).toOption)
        )
        .getOrElse(throw errAt(pos, s"unbound variable: $name"))

    def define(name: String, value: Value): Unit = bindings(name) = value

    def set(name: String, value: Value, pos: Pos): Unit =
      if bindings.contains(name) then bindings(name) = value
      else
        parent match
          case Some(p) => p.set(name, value, pos)
          case None    => throw errAt(pos, s"unbound variable: $name")

    def lookupOpt(name: String): Option[Value] =
      bindings.get(name).orElse(parent.flatMap(_.lookupOpt(name)))

    def child(): Env =
      Env(scala.collection.mutable.Map.empty, Some(this), output)

  // ── Helpers ──────────────────────────────────────────────────────────
  def isTruthy(v: Value): Boolean = v match
    case Value.VBool(false) => false
    case _                  => true

  def asNum(v: Value, pos: Pos): Long = v match
    case Value.VNum(n)         => n
    case Value.VRational(n, d) => n / d
    case Value.VFloat(d)       => d.toLong
    case _                     => throw errAt(pos, "expected number")

  def toDouble(v: Value, pos: Pos): Double = v match
    case Value.VNum(n)         => n.toDouble
    case Value.VFloat(d)       => d
    case Value.VRational(n, d) => n.toDouble / d.toDouble
    case _                     => throw errAt(pos, "expected number")

  def isNumeric(v: Value): Boolean = v match
    case _: Value.VNum | _: Value.VFloat | _: Value.VRational => true
    case _                                                    => false

  def gcd(a: Long, b: Long): Long =
    if b == 0 then math.abs(a) else gcd(b, a % b)

  def makeRational(n: Long, d: Long): Value =
    if d == 0 then throw EvalError("division by zero")
    val g    = gcd(math.abs(n), math.abs(d))
    val sign = if d < 0 then -1L else 1L
    val rn   = sign * n / g
    val rd   = sign * d / g
    if rd == 1L then Value.VNum(rn) else Value.VRational(rn, rd)

  def numericEqual(a: Value, b: Value, pos: Pos): Boolean =
    (a, b) match
      case (Value.VNum(x), Value.VNum(y))                     => x == y
      case (Value.VFloat(x), Value.VFloat(y))                 => x == y
      case (Value.VRational(n1, d1), Value.VRational(n2, d2)) => n1 == n2 && d1 == d2
      case _                                                  => toDouble(a, pos) == toDouble(b, pos)

  def numericCompare(a: Value, b: Value, pos: Pos): Int =
    (a, b) match
      case (Value.VNum(x), Value.VNum(y)) => x.compareTo(y)
      case _                              => toDouble(a, pos).compareTo(toDouble(b, pos))

  def valuesEqual(a: Value, b: Value): Boolean = valuesEqualD(a, b, 0)

  private def valuesEqualD(a: Value, b: Value, depth: Int): Boolean =
    if depth > 100000 then return true // cycle guard
    (a, b) match
      case (Value.VNum(x), Value.VNum(y))                     => x == y
      case (Value.VFloat(x), Value.VFloat(y))                 => x == y
      case (Value.VRational(n1, d1), Value.VRational(n2, d2)) => n1 == n2 && d1 == d2
      case (Value.VBool(x), Value.VBool(y))                   => x == y
      case (Value.VStr(x, _), Value.VStr(y, _))               => java.util.Arrays.equals(x, y)
      case (Value.VChar(x), Value.VChar(y))                   => x == y
      case (Value.VSymbol(x), Value.VSymbol(y))               => x == y
      case (Value.VList(xs), Value.VList(ys)) =>
        xs.length == ys.length && xs
          .zip(ys)
          .forall((a, b) => valuesEqualD(a, b, depth + 1))
      case (Value.VDottedList(xs, xl), Value.VDottedList(ys, yl)) =>
        xs.length == ys.length && xs
          .zip(ys)
          .forall((a, b) => valuesEqualD(a, b, depth + 1)) && valuesEqualD(xl, yl, depth + 1)
      case (Value.VPair(c1), Value.VPair(c2)) =>
        if c1 eq c2 then true
        else valuesEqualD(c1.car, c2.car, depth + 1) && valuesEqualD(c1.cdr, c2.cdr, depth + 1)
      case (Value.VPair(_), Value.VList(_)) | (Value.VList(_), Value.VPair(_)) =>
        // compare structurally: convert both to element lists
        val aElems = toScalaListOpt(a)
        val bElems = toScalaListOpt(b)
        (aElems, bElems) match
          case (Some(xs), Some(ys)) =>
            xs.length == ys.length && xs.zip(ys).forall((a, b) => valuesEqualD(a, b, depth + 1))
          case _ => false
      case (Value.VVector(xs), Value.VVector(ys)) =>
        xs.length == ys.length && xs.zip(ys).forall((a, b) => valuesEqualD(a, b, depth + 1))
      case (Value.VVoid, Value.VVoid) => true
      case _                          => false

  // ── List/pair helpers ────────────────────────────────────────────────
  /** Convert a list-like Value to Scala List. Returns None for cycles or improper lists. */
  def toScalaListOpt(v: Value): Option[List[Value]] =
    val buf  = scala.collection.mutable.ListBuffer[Value]()
    var cur  = v
    val seen = new java.util.IdentityHashMap[PairCell, java.lang.Boolean]()
    while true do
      cur match
        case Value.VList(Nil)   => return Some(buf.toList)
        case Value.VList(elems) => buf ++= elems; return Some(buf.toList)
        case Value.VPair(cell) =>
          if seen.containsKey(cell) then return None
          seen.put(cell, java.lang.Boolean.TRUE)
          buf += cell.car
          cur = cell.cdr
        case _ => return None
    None // unreachable

  /** Convert a list-like Value to Scala List. Throws on cycle or improper list. */
  def pairToScalaList(v: Value, pos: Pos): List[Value] =
    toScalaListOpt(v).getOrElse(throw errAt(pos, "not a proper list"))

  /** Build a scheme list (VPair chain ending in VList(Nil)) from Scala List. */
  def schemeList(elems: List[Value]): Value =
    elems.foldRight(Value.VList(Nil): Value)((h, t) => Value.VPair(new PairCell(h, t)))

  /** Test if value is a proper list (with cycle detection). */
  def isProperList(v: Value): Boolean =
    var cur  = v
    val seen = new java.util.IdentityHashMap[PairCell, java.lang.Boolean]()
    while true do
      cur match
        case Value.VList(_) => return true
        case Value.VPair(cell) =>
          if seen.containsKey(cell) then return false
          seen.put(cell, java.lang.Boolean.TRUE)
          cur = cell.cdr
        case _ => return false
    false
