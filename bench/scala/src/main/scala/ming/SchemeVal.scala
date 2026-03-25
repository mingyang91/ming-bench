package ming

class PairCell(var car: SchemeVal, var cdr: SchemeVal)

enum SchemeVal:
  var pos: (Int, Int)    = (0, 0)
  var immutable: Boolean = false
  case IntVal(n: Long)
  case RationalVal(num: Long, den: Long)
  case FloatVal(d: Double)
  case BoolVal(b: Boolean)
  case StringVal(chars: Array[Char])
  case Symbol(name: String)
  case CharVal(c: Char)
  case SList(elems: List[SchemeVal])
  case DottedList(elems: List[SchemeVal], tail: SchemeVal)
  case Pair(cell: PairCell)
  case Void
  case BuiltinProc(name: String, f: List[SchemeVal] => SchemeVal)
  case LambdaProc(params: List[String], body: List[SchemeVal], closure: Env, rest: Option[String] = None)
  case MacroVal(name: String, literals: List[String], rules: List[(SchemeVal, SchemeVal)], defEnv: Env)
  case CaseLambdaProc(clauses: List[(List[String], Option[String], List[SchemeVal])], closure: Env)
  case RecordVal(typeName: String, fields: scala.collection.mutable.Map[String, SchemeVal])
  case VectorVal(elems: Array[SchemeVal])
  case TailCall(expr: SchemeVal, env: Env)

object SchemeVal:

  def str(s: String): StringVal = StringVal(s.toCharArray)

  def makePair(car: SchemeVal, cdr: SchemeVal): Pair = Pair(PairCell(car, cdr))

  /** Convert a list-like value (SList or Pair chain) to a Scala List. Throws on improper lists. */
  def toScalaList(v: SchemeVal): List[SchemeVal] =
    v match
      case SList(elems) => elems
      case Pair(_) =>
        val buf            = scala.collection.mutable.ListBuffer[SchemeVal]()
        var cur: SchemeVal = v
        while true do
          cur match
            case Pair(c) =>
              buf += c.car
              cur = c.cdr
            case SList(Nil)   => return buf.toList
            case SList(elems) => return (buf ++= elems).toList
            case other        => throw new EvalError(s"expected proper list, got ${display(other)}")
        buf.toList // unreachable
      case other => throw new EvalError(s"expected list, got ${display(other)}")

  /** Check if value is a proper list (SList or Pair chain ending in nil), with cycle detection. */
  def isList(v: SchemeVal): Boolean =
    v match
      case SList(_) => true
      case Pair(_)  =>
        // Tortoise and hare cycle detection
        var slow: SchemeVal = v
        var fast: SchemeVal = v
        while true do
          fast match
            case Pair(fc) =>
              fc.cdr match
                case Pair(fc2) =>
                  fast = fc2.cdr
                  slow = slow match
                    case Pair(sc) => sc.cdr
                    case _        => return true // shouldn't happen
                case SList(Nil) => return true
                case SList(_)   => return true
                case _          => return false // improper
              // Check if slow == fast (cycle)
              (slow, fast) match
                case (Pair(s), Pair(f)) if s eq f => return false // cycle detected
                case _                            => ()
            case SList(Nil) => return true
            case SList(_)   => return true
            case _          => return false
        false // unreachable
      case _ => false

  private def gcd(a: Long, b: Long): Long =
    val aa = math.abs(a); val bb = math.abs(b)
    if bb == 0 then aa else gcd(bb, aa % bb)

  def makeRational(num: Long, den: Long): SchemeVal =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1L else 1L
    val n    = num * sign; val d = den * sign
    val g    = gcd(math.abs(n), d)
    val sn   = n / g; val sd     = d / g
    if sd == 1 then IntVal(sn) else RationalVal(sn, sd)

  def toDouble(v: SchemeVal): Double = v match
    case IntVal(n)         => n.toDouble
    case RationalVal(n, d) => n.toDouble / d.toDouble
    case FloatVal(d)       => d
    case other             => throw new EvalError(s"expected number, got ${display(other)}")

  def isNumeric(v: SchemeVal): Boolean = v match
    case IntVal(_) | RationalVal(_, _) | FloatVal(_) => true
    case _                                           => false

  def isExact(v: SchemeVal): Boolean = v match
    case IntVal(_) | RationalVal(_, _) => true
    case _                             => false

  /** write-style display (strings quoted), cycle-safe */
  def display(v: SchemeVal): String =
    val seen = new java.util.IdentityHashMap[PairCell, java.lang.Boolean]()
    displayImpl(v, seen, quoted = true)

  /** display-style output (strings unquoted), cycle-safe */
  def displayOutput(v: SchemeVal): String =
    val seen = new java.util.IdentityHashMap[PairCell, java.lang.Boolean]()
    displayImpl(v, seen, quoted = false)

  private def displayImpl(
    v: SchemeVal,
    seen: java.util.IdentityHashMap[PairCell, java.lang.Boolean],
    quoted: Boolean
  ): String =
    v match
      case IntVal(n)         => n.toString
      case RationalVal(n, d) => s"$n/$d"
      case FloatVal(d)       => if d == d.toLong.toDouble && !d.isInfinite then s"${d.toLong}.0" else d.toString
      case BoolVal(true)     => "#t"
      case BoolVal(false)    => "#f"
      case StringVal(chars)  => if quoted then s"\"${new String(chars)}\"" else new String(chars)
      case CharVal(' ')      => if quoted then "#\\space" else " "
      case CharVal('\n')     => if quoted then "#\\newline" else "\n"
      case CharVal('\t')     => if quoted then "#\\tab" else "\t"
      case CharVal(c)        => if quoted then s"#\\$c" else c.toString
      case Symbol(name)      => name
      case SList(elems)      => "(" + elems.map(e => displayImpl(e, seen, quoted)).mkString(" ") + ")"
      case DottedList(elems, tail) =>
        "(" + elems.map(e => displayImpl(e, seen, quoted)).mkString(" ") + " . " + displayImpl(tail, seen, quoted) + ")"
      case Pair(cell) =>
        if seen.containsKey(cell) then return "(...)";
        seen.put(cell, java.lang.Boolean.TRUE)
        val sb = new StringBuilder("(")
        sb.append(displayImpl(cell.car, seen, quoted))
        var cur  = cell.cdr
        var done = false
        while !done do
          cur match
            case Pair(c) =>
              if seen.containsKey(c) then
                sb.append(" ...")
                done = true
              else
                seen.put(c, java.lang.Boolean.TRUE)
                sb.append(" ")
                sb.append(displayImpl(c.car, seen, quoted))
                cur = c.cdr
            case SList(Nil) =>
              done = true
            case SList(elems) =>
              elems.foreach { e =>
                sb.append(" ")
                sb.append(displayImpl(e, seen, quoted))
              }
              done = true
            case other =>
              sb.append(" . ")
              sb.append(displayImpl(other, seen, quoted))
              done = true
        sb.append(")")
        sb.toString
      case Void                    => ""
      case BuiltinProc(name, _)    => s"#<procedure $name>"
      case LambdaProc(_, _, _, _)  => "#<procedure>"
      case CaseLambdaProc(_, _)    => "#<procedure>"
      case MacroVal(name, _, _, _) => s"#<macro $name>"
      case RecordVal(typeName, _)  => s"#<$typeName>"
      case VectorVal(elems)        => "#(" + elems.map(e => displayImpl(e, seen, quoted)).mkString(" ") + ")"
      case TailCall(_, _)          => "#<tailcall>"

  private class IdentityHashSet:
    private val map                    = new java.util.IdentityHashMap[AnyRef, java.lang.Boolean]()
    def contains(key: AnyRef): Boolean = map.containsKey(key)
    def add(key: AnyRef): Unit         = map.put(key, java.lang.Boolean.TRUE)

  def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean =
    val seen = new IdentityHashSet()
    schemeEqualImpl(a, b, seen)

  private def schemeEqualImpl(a: SchemeVal, b: SchemeVal, seen: IdentityHashSet): Boolean = (a, b) match
    case (IntVal(x), IntVal(y))                     => x == y
    case (RationalVal(n1, d1), RationalVal(n2, d2)) => n1 == n2 && d1 == d2
    case (FloatVal(x), FloatVal(y))                 => x == y
    case (BoolVal(x), BoolVal(y))                   => x == y
    case (StringVal(x), StringVal(y))               => java.util.Arrays.equals(x, y)
    case (CharVal(x), CharVal(y))                   => x == y
    case (Symbol(x), Symbol(y))                     => x == y
    case (SList(xs), SList(ys)) => xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqualImpl(a, b, seen))
    case (DottedList(xs, xt), DottedList(ys, yt)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqualImpl(a, b, seen)) && schemeEqualImpl(
        xt,
        yt,
        seen
      )
    case (VectorVal(xs), VectorVal(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqualImpl(a, b, seen))
    case (Void, Void) => true
    // Pair vs SList: compare structurally
    case _ if isPairLike(a) && isPairLike(b) =>
      comparePairLike(a, b, seen)
    case _ => false

  private def isPairLike(v: SchemeVal): Boolean = v match
    case Pair(_)                        => true
    case SList(elems) if elems.nonEmpty => true
    case DottedList(_, _)               => true
    case _                              => false

  private def comparePairLike(a: SchemeVal, b: SchemeVal, seen: IdentityHashSet): Boolean =
    val carA = pairCar(a)
    val cdrA = pairCdr(a)
    val carB = pairCar(b)
    val cdrB = pairCdr(b)
    schemeEqualImpl(carA, carB, seen) && schemeEqualImpl(cdrA, cdrB, seen)

  private def pairCar(v: SchemeVal): SchemeVal = v match
    case Pair(c)                                => c.car
    case SList(elems) if elems.nonEmpty         => elems.head
    case DottedList(elems, _) if elems.nonEmpty => elems.head
    case _                                      => throw new EvalError("car: not a pair")

  private def pairCdr(v: SchemeVal): SchemeVal = v match
    case Pair(c)                        => c.cdr
    case SList(elems) if elems.nonEmpty => SList(elems.tail)
    case DottedList(elems, tail) if elems.nonEmpty =>
      if elems.tail.isEmpty then tail else DottedList(elems.tail, tail)
    case _ => throw new EvalError("cdr: not a pair")

class Env(
  private val bindings: scala.collection.mutable.Map[String, SchemeVal],
  private val parent: Option[Env]
):

  def lookup(name: String): Option[SchemeVal] =
    bindings.get(name).orElse(parent.flatMap(_.lookup(name)))

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

  def set(name: String, value: SchemeVal): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value)
        case None    => throw new EvalError(s"set!: unbound variable: $name")

object Env:

  def default(): Env =
    val env = new Env(scala.collection.mutable.Map.empty, None)
    Builtins.register(env)
    env

  def defaultWithOutput(output: StringBuilder): Env =
    val env = new Env(scala.collection.mutable.Map.empty, None)
    Builtins.register(env, output)
    env
