package ming

object SchemeTypes:

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
    case VVoid

  type Pos = ming.Pos

  def display(v: Value): String = v match
    case Value.VNum(n)         => n.toString
    case Value.VFloat(d)       => displayFloat(d)
    case Value.VRational(n, d) => s"$n/$d"
    case Value.VBool(true)     => "#t"
    case Value.VBool(false)    => "#f"
    case Value.VStr(chars, _)  => s"\"${new String(chars)}\""
    case Value.VChar(c)        => displayChar(c)
    case Value.VList(elems) =>
      "(" + elems.map(display).mkString(" ") + ")"
    case Value.VDottedList(elems, last) =>
      "(" + elems.map(display).mkString(" ") + " . " + display(last) + ")"
    case Value.VSymbol(n)          => n
    case Value.VBuiltin(n)         => s"#<procedure $n>"
    case Value.VLambda(_, _, _, _) => "#<procedure>"
    case Value.VCaseLambda(_)      => "#<procedure>"
    case Value.VMacro(_, _, _)     => "#<macro>"
    case Value.VVector(elems) =>
      "#(" + elems.map(display).mkString(" ") + ")"
    case Value.VRecord(typeName, _) => s"#<record:$typeName>"
    case Value.VVoid                => ""

  private def displayFloat(d: Double): String =
    if d == d.toLong.toDouble && !d.isInfinite then s"${d.toLong}.0"
    else d.toString

  private def displayChar(c: Char): String = c match
    case ' '  => "#\\space"
    case '\n' => "#\\newline"
    case '\t' => "#\\tab"
    case _    => s"#\\$c"

  def displayStr(v: Value): String = v match
    case Value.VStr(chars, _) => new String(chars)
    case Value.VChar(c)    => c.toString
    case Value.VList(elems) =>
      "(" + elems.map(displayStr).mkString(" ") + ")"
    case Value.VDottedList(elems, last) =>
      "(" + elems.map(displayStr).mkString(" ") + " . " + displayStr(
        last
      ) + ")"
    case Value.VVector(elems) =>
      "#(" + elems.map(displayStr).mkString(" ") + ")"
    case other => display(other)

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

  def valuesEqual(a: Value, b: Value): Boolean = (a, b) match
    case (Value.VNum(x), Value.VNum(y))                     => x == y
    case (Value.VFloat(x), Value.VFloat(y))                 => x == y
    case (Value.VRational(n1, d1), Value.VRational(n2, d2)) => n1 == n2 && d1 == d2
    case (Value.VBool(x), Value.VBool(y))                   => x == y
    case (Value.VStr(x, _), Value.VStr(y, _))                => java.util.Arrays.equals(x, y)
    case (Value.VChar(x), Value.VChar(y))                   => x == y
    case (Value.VSymbol(x), Value.VSymbol(y))               => x == y
    case (Value.VList(xs), Value.VList(ys)) =>
      xs.length == ys.length && xs
        .zip(ys)
        .forall((a, b) => valuesEqual(a, b))
    case (Value.VDottedList(xs, xl), Value.VDottedList(ys, yl)) =>
      xs.length == ys.length && xs
        .zip(ys)
        .forall((a, b) => valuesEqual(a, b)) && valuesEqual(xl, yl)
    case (Value.VVector(xs), Value.VVector(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => valuesEqual(a, b))
    case (Value.VVoid, Value.VVoid) => true
    case _                          => false

  val builtinNames: List[String] = List(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "append",
    "number?",
    "string?",
    "boolean?",
    "pair?",
    "symbol?",
    "char?",
    "display",
    "write",
    "newline",
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "symbol->string",
    "string->symbol",
    "string-ref",
    "string-copy",
    "string-set!",
    "apply",
    "abs",
    "modulo",
    "remainder",
    "quotient",
    "min",
    "max",
    "expt",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "list-ref",
    "list-tail",
    "list?",
    "assoc",
    "map",
    "equal?",
    "eq?",
    "char-alphabetic?",
    "char-numeric?",
    "char-upcase",
    "char-downcase",
    "char=?",
    "char<?",
    "string=?",
    "string<?",
    "string-ci=?",
    "string-upcase",
    "string-downcase",
    "exact?",
    "inexact?",
    "exact->inexact",
    "inexact->exact",
    "numerator",
    "denominator",
    "rational?",
    "integer?",
    "procedure?",
    "eqv?",
    "vector",
    "make-vector",
    "vector-ref",
    "vector-set!",
    "vector-length",
    "vector?",
    "vector->list",
    "list->vector",
    "string->list",
    "list->string",
    "char->integer",
    "integer->char"
  )
