package ming

import scala.collection.mutable.ListBuffer

/** Scheme value types */
enum SchemeVal:
  case SInt(value: Long)
  case SBool(value: Boolean)
  case SString(value: String)
  case SSymbol(name: String)
  case SList(elems: List[SchemeVal])
  case SVoid

  def display: String = this match
    case SInt(v)    => v.toString
    case SBool(v)   => if v then "#t" else "#f"
    case SString(v) => s""""$v""""
    case SSymbol(n) => n
    case SList(es)  => "(" + es.map(_.display).mkString(" ") + ")"
    case SVoid      => ""

/** Tokenizer */
object Tokenizer:

  def tokenize(input: String): List[String] =
    val tokens = ListBuffer[String]()
    var i      = 0
    while i < input.length do i = tokenizeOne(input, i, tokens)
    tokens.toList

  private def tokenizeOne(input: String, pos: Int, tokens: ListBuffer[String]): Int =
    input(pos) match
      case c if c.isWhitespace => pos + 1
      case ';'                 => skipLineComment(input, pos + 1)
      case '('                 => tokens += "("; pos + 1
      case ')'                 => tokens += ")"; pos + 1
      case '\''                => tokens += "'"; pos + 1
      case '"' =>
        val (tok, next) = tokenizeString(input, pos)
        tokens += tok
        next
      case '#' => tokenizeHash(input, pos, tokens)
      case _   => tokenizeWord(input, pos, tokens)

  private def skipLineComment(input: String, start: Int): Int =
    var i = start
    while i < input.length && input(i) != '\n' do i += 1
    i

  private def tokenizeHash(input: String, pos: Int, tokens: ListBuffer[String]): Int =
    var i = pos + 1
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do i += 1
    tokens += input.substring(pos, i)
    i

  private def tokenizeWord(input: String, pos: Int, tokens: ListBuffer[String]): Int =
    var i = pos
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' && input(i) != '"' && input(
        i
      ) != ';'
    do i += 1
    tokens += input.substring(pos, i)
    i

  private def tokenizeString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start + 1
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        sb += input(i)
        i += 1
        if i < input.length then
          sb += input(i)
          i += 1
      else
        sb += input(i)
        i += 1
    if i < input.length then
      sb += '"'
      i += 1
    (sb.toString, i)

/** Parser */
object Parser:

  def parse(tokens: List[String]): (SchemeVal, List[String]) =
    tokens match
      case Nil => throw new EvalError("unexpected end of input")
      case "(" :: rest =>
        val elems     = ListBuffer[SchemeVal]()
        var remaining = rest
        while remaining.nonEmpty && remaining.head != ")" do
          val (expr, r) = parse(remaining)
          elems += expr
          remaining = r
        if remaining.isEmpty then throw new EvalError("missing closing parenthesis")
        (SchemeVal.SList(elems.toList), remaining.tail)
      case ")" :: _ => throw new EvalError("unexpected )")
      case "'" :: rest =>
        val (expr, r) = parse(rest)
        (SchemeVal.SList(List(SchemeVal.SSymbol("quote"), expr)), r)
      case token :: rest =>
        (parseAtom(token), rest)

  private def parseAtom(token: String): SchemeVal =
    if token == "#t" then SchemeVal.SBool(true)
    else if token == "#f" then SchemeVal.SBool(false)
    else if token.startsWith("\"") && token.endsWith("\"") then
      SchemeVal.SString(unescapeString(token.substring(1, token.length - 1)))
    else
      token.toLongOption match
        case Some(n) => SchemeVal.SInt(n)
        case None    => SchemeVal.SSymbol(token)

  private def unescapeString(s: String): String =
    val sb = new StringBuilder
    var i  = 0
    while i < s.length do
      if s(i) == '\\' && i + 1 < s.length then
        s(i + 1) match
          case 'n'   => sb += '\n'; i += 2
          case 't'   => sb += '\t'; i += 2
          case '\\'  => sb += '\\'; i += 2
          case '"'   => sb += '"'; i += 2
          case other => sb += '\\'; sb += other; i += 2
      else
        sb += s(i)
        i += 1
    sb.toString

  def parseAll(input: String): List[SchemeVal] =
    val tokens    = Tokenizer.tokenize(input)
    val exprs     = ListBuffer[SchemeVal]()
    var remaining = tokens
    while remaining.nonEmpty do
      val (expr, r) = parse(remaining)
      exprs += expr
      remaining = r
    exprs.toList

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  private val builtinNames = Set("+", "-", "*", "/", "=", "<", ">", "<=", ">=", "not")

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  def eval(expr: SchemeVal): SchemeVal = expr match
    case SchemeVal.SInt(_) | SchemeVal.SBool(_) | SchemeVal.SString(_) | SchemeVal.SVoid => expr
    case s @ SchemeVal.SSymbol(name) =>
      if builtinNames.contains(name) then s
      else throw new EvalError(s"unbound variable: $name")
    case SchemeVal.SList(elems) =>
      elems match
        case Nil                              => throw new EvalError("empty application")
        case SchemeVal.SSymbol("and") :: args => evalAnd(args)
        case SchemeVal.SSymbol("or") :: args  => evalOr(args)
        case head :: args =>
          val op         = eval(head)
          val evaledArgs = args.map(eval)
          applyBuiltin(op, evaledArgs)

  private def evalAnd(args: List[SchemeVal]): SchemeVal =
    if args.isEmpty then SchemeVal.SBool(true)
    else
      var result: SchemeVal = SchemeVal.SBool(true)
      val iter              = args.iterator
      var done              = false
      while iter.hasNext && !done do
        result = eval(iter.next())
        if !isTruthy(result) then done = true
      result

  private def evalOr(args: List[SchemeVal]): SchemeVal =
    if args.isEmpty then SchemeVal.SBool(false)
    else
      var result: SchemeVal = SchemeVal.SBool(false)
      val iter              = args.iterator
      var found             = false
      while iter.hasNext && !found do
        result = eval(iter.next())
        if isTruthy(result) then found = true
      result

  private def applyBuiltin(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeVal.SSymbol(name) =>
        name match
          case "+" =>
            val nums = args.map(asInt)
            SchemeVal.SInt(nums.sum)
          case "-" =>
            if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
            val nums = args.map(asInt)
            if nums.length == 1 then SchemeVal.SInt(-nums.head)
            else SchemeVal.SInt(nums.head - nums.tail.sum)
          case "*" =>
            val nums = args.map(asInt)
            SchemeVal.SInt(nums.product)
          case "/" =>
            if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
            val nums = args.map(asInt)
            if nums.tail.contains(0L) then throw new EvalError("division by zero")
            SchemeVal.SInt(nums.head / nums.tail.product)
          case "=" =>
            if args.length < 2 then throw new EvalError("=: expected at least 2 arguments")
            val nums = args.map(asInt)
            SchemeVal.SBool(nums.forall(_ == nums.head))
          case "<" =>
            if args.length < 2 then throw new EvalError("<: expected at least 2 arguments")
            val nums = args.map(asInt)
            SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a < b))
          case ">" =>
            if args.length < 2 then throw new EvalError(">: expected at least 2 arguments")
            val nums = args.map(asInt)
            SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a > b))
          case "<=" =>
            if args.length < 2 then throw new EvalError("<=: expected at least 2 arguments")
            val nums = args.map(asInt)
            SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a <= b))
          case ">=" =>
            if args.length < 2 then throw new EvalError(">=: expected at least 2 arguments")
            val nums = args.map(asInt)
            SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a >= b))
          case "not" =>
            if args.length != 1 then throw new EvalError("not: expected 1 argument")
            SchemeVal.SBool(!isTruthy(args.head))
          case other =>
            throw new EvalError(s"unknown procedure: $other")
      case _ =>
        throw new EvalError(s"not a procedure: ${op.display}")

  private def asInt(v: SchemeVal): Long = v match
    case SchemeVal.SInt(n) => n
    case other             => throw new EvalError(s"expected number, got ${other.display}")

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    var result: SchemeVal = SchemeVal.SVoid
    for expr <- exprs do result = eval(expr)
    result.display

  /** Evaluate Scheme expressions and return both the result string and any captured output. */
  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")
