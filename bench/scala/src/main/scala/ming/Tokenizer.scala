package ming

enum Token:
  case LParen(pos: (Int, Int))
  case RParen(pos: (Int, Int))
  case VecLParen(pos: (Int, Int))
  case Str(value: String, pos: (Int, Int))
  case Atom(value: String, pos: (Int, Int))

object Tokenizer:

  def tokenize(input: String): List[Token] =
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    // Precompute line:col for each character index
    val positions = new Array[(Int, Int)](input.length + 1)
    var line      = 1
    var col       = 1
    for i <- input.indices do
      positions(i) = (line, col)
      if input(i) == '\n' then
        line += 1; col = 1
      else col += 1
    positions(input.length) = (line, col)
    var i = 0
    while i < input.length do i = readToken(input, i, positions, tokens)
    tokens.toList

  private def readToken(
    input: String,
    pos: Int,
    positions: Array[(Int, Int)],
    tokens: scala.collection.mutable.ListBuffer[Token]
  ): Int =
    val ch = input(pos)
    ch match
      case _ if ch.isWhitespace => pos + 1
      case ';' =>
        var i = pos
        while i < input.length && input(i) != '\n' do i += 1
        i
      case '(' =>
        tokens += Token.LParen(positions(pos))
        pos + 1
      case ')' =>
        tokens += Token.RParen(positions(pos))
        pos + 1
      case '"' => readString(input, pos + 1, positions(pos), tokens)
      case '\'' =>
        tokens += Token.Atom("quote-sugar", positions(pos))
        pos + 1
      case '`' =>
        tokens += Token.Atom("quasiquote-sugar", positions(pos))
        pos + 1
      case ',' =>
        if pos + 1 < input.length && input(pos + 1) == '@' then
          tokens += Token.Atom("unquote-splicing-sugar", positions(pos))
          pos + 2
        else
          tokens += Token.Atom("unquote-sugar", positions(pos))
          pos + 1
      case '#' if pos + 1 < input.length && input(pos + 1) == '(' =>
        tokens += Token.VecLParen(positions(pos))
        pos + 2
      case '#' if pos + 1 < input.length && input(pos + 1) == '\'' =>
        tokens += Token.Atom("syntax-sugar", positions(pos))
        pos + 2
      case _ =>
        val sb       = new StringBuilder
        val startPos = positions(pos)
        var i        = pos
        while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' && input(
            i
          ) != ';' && input(i) != '"'
        do
          sb += input(i)
          i += 1
        tokens += Token.Atom(sb.toString, startPos)
        i

  private def readString(
    input: String,
    start: Int,
    startPos: (Int, Int),
    tokens: scala.collection.mutable.ListBuffer[Token]
  ): Int =
    val sb = new StringBuilder
    var i  = start
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' && i + 1 < input.length then
        input(i + 1) match
          case 'n'   => sb += '\n'; i += 2
          case 't'   => sb += '\t'; i += 2
          case '\\'  => sb += '\\'; i += 2
          case '"'   => sb += '"'; i += 2
          case other => sb += '\\'; sb += other; i += 2
      else
        sb += input(i)
        i += 1
    if i < input.length then i += 1 // skip closing "
    tokens += Token.Str(sb.toString, startPos)
    i
