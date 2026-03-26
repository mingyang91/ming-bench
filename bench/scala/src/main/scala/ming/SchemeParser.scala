package ming

import java.util.Arrays
import scala.collection.mutable.ListBuffer

import SchemeModel.*

private[ming] object SchemeParser:

  def parseProgram(input: String): List[Expr] =
    Parser(input).parseProgram()

  final private class Parser(input: String):
    private var index = 0

    private val lineStarts: Array[Int] =
      val starts = ListBuffer(0)
      var offset = 0
      while offset < input.length do
        if input.charAt(offset) == '\n' then starts += offset + 1
        offset += 1
      starts.toArray

    def parseProgram(): List[Expr] =
      val expressions = ListBuffer.empty[Expr]
      skipTrivia()
      while !isAtEnd do
        expressions += parseExpr()
        skipTrivia()
      expressions.toList

    private def parseExpr(): Expr =
      skipTrivia()
      if isAtEnd then parseError("unexpected end of input")

      val pos = positionFor(index)
      input.charAt(index) match
        case '(' =>
          index += 1
          parseList(pos)
        case '\'' =>
          index += 1
          Expr.ListExpr(List(Expr.Symbol("quote", pos), parseExpr()), pos)
        case ')' =>
          parseError("unexpected ')'")
        case '"' =>
          parseString(pos)
        case '#' =>
          parseHashLiteral(pos)
        case _ =>
          parseAtom(pos)

    private def parseHashLiteral(pos: SourcePos): Expr =
      if startsWith("#\\") then parseChar(pos)
      else parseBoolean(pos)

    private def parseList(pos: SourcePos): Expr =
      val items = ListBuffer.empty[Expr]
      skipTrivia()
      while !isAtEnd && input.charAt(index) != ')' do
        items += parseExpr()
        skipTrivia()

      if isAtEnd then parseError("unterminated list")
      index += 1
      Expr.ListExpr(items.toList, pos)

    private def parseString(pos: SourcePos): Expr =
      index += 1
      val builder = new StringBuilder

      while !isAtEnd && input.charAt(index) != '"' do
        val char = input.charAt(index)
        if char == '\\' then
          index += 1
          if isAtEnd then parseError("unterminated string escape")
          val escaped = input.charAt(index)
          builder +=
            (escaped match
              case '"'   => '"'
              case '\\'  => '\\'
              case 'n'   => '\n'
              case 'r'   => '\r'
              case 't'   => '\t'
              case other => other)
        else builder += char
        index += 1

      if isAtEnd then parseError("unterminated string literal")
      index += 1
      Expr.StringLiteral(builder.result(), pos)

    private def parseBoolean(pos: SourcePos): Expr =
      if startsWith("#t") && tokenBoundary(index + 2) then
        index += 2
        Expr.BooleanLiteral(true, pos)
      else if startsWith("#f") && tokenBoundary(index + 2) then
        index += 2
        Expr.BooleanLiteral(false, pos)
      else parseError("invalid boolean literal")

    private def parseChar(pos: SourcePos): Expr =
      index += 2
      if isAtEnd || isDelimiter(input.charAt(index)) then parseError("invalid character literal")

      val start = index
      while !isAtEnd && !isDelimiter(input.charAt(index)) do index += 1

      val token      = input.substring(start, index)
      val normalized = token.toLowerCase(java.util.Locale.ROOT)
      val codePoint =
        normalized match
          case "space"   => 32
          case "newline" => 10
          case _ if token.codePointCount(0, token.length) == 1 =>
            token.codePointAt(0)
          case _ =>
            parseError("invalid character literal")

      Expr.CharLiteral(codePoint, pos)

    private def parseAtom(pos: SourcePos): Expr =
      val start = index
      while !isAtEnd && !isDelimiter(input.charAt(index)) do index += 1

      val token = input.substring(start, index)
      if token.matches("-?\\d+") then Expr.IntegerLiteral(BigInt(token), pos)
      else Expr.Symbol(token, pos)

    private def skipTrivia(): Unit =
      var keepSkipping = true
      while keepSkipping && !isAtEnd do
        while !isAtEnd && input.charAt(index).isWhitespace do index += 1

        if !isAtEnd && input.charAt(index) == ';' then while !isAtEnd && input.charAt(index) != '\n' do index += 1
        else keepSkipping = false

    private def startsWith(prefix: String): Boolean =
      input.regionMatches(index, prefix, 0, prefix.length)

    private def tokenBoundary(boundary: Int): Boolean =
      boundary >= input.length || isDelimiter(input.charAt(boundary))

    private def isDelimiter(char: Char): Boolean =
      char.isWhitespace || char == '(' || char == ')' || char == ';'

    private def isAtEnd: Boolean =
      index >= input.length

    private def positionFor(offset: Int): SourcePos =
      val indexInLineStarts = Arrays.binarySearch(lineStarts, offset)
      val lineIndex =
        if indexInLineStarts >= 0 then indexInLineStarts
        else -indexInLineStarts - 2
      SourcePos(lineIndex + 1, offset - lineStarts(lineIndex) + 1)

    private def parseError(message: String): Nothing =
      throw EvalError.at(message, positionFor(index))
