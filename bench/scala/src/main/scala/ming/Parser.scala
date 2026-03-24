package ming

import Evaluator.Val
import Evaluator.Val.*

/** S-expression parser for Scheme source code. */
private[ming] class Parser(input: String):
  private var pos  = 0
  private var line = 1
  private var col  = 1

  def parseAll(): List[Val] =
    parseAllWithPositions().map(_._1)

  def parseAllWithPositions(): List[(Val, Int, Int)] =
    val exprs = scala.collection.mutable.ListBuffer[(Val, Int, Int)]()
    while
      skipWhitespace()
      pos < input.length
    do
      val startLine = line
      val startCol  = col
      exprs += ((parseExpr(), startLine, startCol))
    exprs.toList

  private def advance(): Char =
    val c = input(pos)
    pos += 1
    if c == '\n' then
      line += 1
      col = 1
    else col += 1
    c

  private def skipWhitespace(): Unit =
    while pos < input.length && (input(pos).isWhitespace || input(pos) == ';') do
      if input(pos) == ';' then while pos < input.length && input(pos) != '\n' do advance()
      else advance()

  private def parseExpr(): Val =
    skipWhitespace()
    if pos >= input.length then throw new EvalError("unexpected end of input")
    input(pos) match
      case '(' =>
        advance()
        parseList()
      case '\'' =>
        advance()
        val e = parseExpr()
        Pair(Symbol("quote"), Pair(e, Nil))
      case '"' =>
        parseString()
      case '#' =>
        advance()
        if pos >= input.length then throw new EvalError("unexpected end of input after #")
        input(pos) match
          case 't' => advance(); Bool(true)
          case 'f' => advance(); Bool(false)
          case '\\' =>
            advance() // skip backslash
            if pos >= input.length then throw new EvalError("unexpected end of input in character literal")
            // Check for named characters
            val startCh = pos
            if input(pos).isLetter then
              while pos < input.length && input(pos).isLetter do advance()
              val name = input.substring(startCh, pos)
              if name.length == 1 then SchemeChar(name.charAt(0))
              else
                name.toLowerCase match
                  case "space"   => SchemeChar(' ')
                  case "newline" => SchemeChar('\n')
                  case "tab"     => SchemeChar('\t')
                  case _         => throw new EvalError(s"unknown character name: $name")
            else
              val c = input(pos)
              advance()
              SchemeChar(c)
          case '(' =>
            advance() // skip (
            val elems = scala.collection.mutable.ArrayBuffer[Val]()
            skipWhitespace()
            while pos < input.length && input(pos) != ')' do
              elems += parseExpr()
              skipWhitespace()
            if pos >= input.length then throw new EvalError("unterminated vector literal")
            advance() // skip )
            Evaluator.Val.Vector(elems.toArray)
          case other => throw new EvalError(s"unexpected character after #: $other")
      case _ =>
        parseAtom()

  private def parseList(): Val =
    skipWhitespace()
    if pos >= input.length then throw new EvalError("unexpected end of input in list")
    if input(pos) == ')' then
      advance()
      Nil
    else
      val first = parseExpr()
      skipWhitespace()
      if pos < input.length && input(pos) == '.' && (pos + 1 >= input.length || input(pos + 1).isWhitespace || input(
          pos + 1
        ) == ')')
      then
        advance()
        val rest = parseExpr()
        skipWhitespace()
        if pos >= input.length || input(pos) != ')' then throw new EvalError("expected ) after dotted pair")
        advance()
        Pair(first, rest)
      else
        val rest = parseList()
        Pair(first, rest)

  private def parseString(): Val =
    advance() // skip opening "
    val sb = new StringBuilder
    while pos < input.length && input(pos) != '"' do
      if input(pos) == '\\' then
        advance()
        if pos >= input.length then throw new EvalError("unterminated string")
        input(pos) match
          case 'n'  => sb += '\n'
          case 't'  => sb += '\t'
          case '\\' => sb += '\\'
          case '"'  => sb += '"'
          case c    => sb += '\\'; sb += c
      else sb += input(pos)
      advance()
    if pos >= input.length then throw new EvalError("unterminated string")
    advance() // skip closing "
    Str(sb.toString.toCharArray)

  private def parseAtom(): Val =
    val start = pos
    while pos < input.length && !input(pos).isWhitespace && !"()\"';".contains(input(pos)) do advance()
    val token = input.substring(start, pos)
    if token.isEmpty then throw new EvalError(s"unexpected character: ${input(pos)}")
    token.toLongOption match
      case Some(n) => Num(n)
      case None    =>
        // Try rational literal: num/den
        val slashIdx = token.indexOf('/')
        if slashIdx > 0 && slashIdx < token.length - 1 then
          val numPart = token.substring(0, slashIdx)
          val denPart = token.substring(slashIdx + 1)
          (numPart.toLongOption, denPart.toLongOption) match
            case (Some(n), Some(d)) =>
              if d == 0 then throw new EvalError("division by zero in rational literal")
              Evaluator.mkRational(n, d)
            case _ => Symbol(token)
        else
          token.toDoubleOption match
            case Some(d) => Inexact(d)
            case None    => Symbol(token)
