package ming

object SchemeTypes:

  // ── Values ───────────────────────────────────────────────────────────
  enum Value:
    case VNum(n: Long)
    case VBool(b: Boolean)
    case VStr(chars: Array[Char])
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
    case VVoid

  type Pos = ming.Pos

  def display(v: Value): String = v match
    case Value.VNum(n)      => n.toString
    case Value.VBool(true)  => "#t"
    case Value.VBool(false) => "#f"
    case Value.VStr(chars)  => s"\"${new String(chars)}\""
    case Value.VChar(c)     => displayChar(c)
    case Value.VList(elems) =>
      "(" + elems.map(display).mkString(" ") + ")"
    case Value.VDottedList(elems, last) =>
      "(" + elems.map(display).mkString(" ") + " . " + display(last) + ")"
    case Value.VSymbol(n)          => n
    case Value.VBuiltin(n)         => s"#<procedure $n>"
    case Value.VLambda(_, _, _, _) => "#<procedure>"
    case Value.VVoid               => ""

  private def displayChar(c: Char): String = c match
    case ' '  => "#\\space"
    case '\n' => "#\\newline"
    case '\t' => "#\\tab"
    case _    => s"#\\$c"

  def displayStr(v: Value): String = v match
    case Value.VStr(chars) => new String(chars)
    case Value.VChar(c)    => c.toString
    case Value.VList(elems) =>
      "(" + elems.map(displayStr).mkString(" ") + ")"
    case Value.VDottedList(elems, last) =>
      "(" + elems.map(displayStr).mkString(" ") + " . " + displayStr(
        last
      ) + ")"
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

    def child(): Env =
      Env(scala.collection.mutable.Map.empty, Some(this), output)

  // ── Helpers ──────────────────────────────────────────────────────────
  def isTruthy(v: Value): Boolean = v match
    case Value.VBool(false) => false
    case _                  => true

  def asNum(v: Value, pos: Pos): Long = v match
    case Value.VNum(n) => n
    case _             => throw errAt(pos, "expected number")

  def valuesEqual(a: Value, b: Value): Boolean = (a, b) match
    case (Value.VNum(x), Value.VNum(y))       => x == y
    case (Value.VBool(x), Value.VBool(y))     => x == y
    case (Value.VStr(x), Value.VStr(y))       => java.util.Arrays.equals(x, y)
    case (Value.VChar(x), Value.VChar(y))     => x == y
    case (Value.VSymbol(x), Value.VSymbol(y)) => x == y
    case (Value.VList(xs), Value.VList(ys)) =>
      xs.length == ys.length && xs
        .zip(ys)
        .forall((a, b) => valuesEqual(a, b))
    case (Value.VDottedList(xs, xl), Value.VDottedList(ys, yl)) =>
      xs.length == ys.length && xs
        .zip(ys)
        .forall((a, b) => valuesEqual(a, b)) && valuesEqual(xl, yl)
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
    "string-downcase"
  )
