package ming

enum Token:
  case LParen
  case RParen
  case Str(value: String)
  case Atom(value: String)

object Tokenizer:

  def tokenize(input: String): List[Token] =
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    var i      = 0
    while i < input.length do i = readToken(input, i, tokens)
    tokens.toList

  private def readToken(
    input: String,
    pos: Int,
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
        tokens += Token.LParen
        pos + 1
      case ')' =>
        tokens += Token.RParen
        pos + 1
      case '"' => readString(input, pos + 1, tokens)
      case '\'' =>
        tokens += Token.Atom("quote-sugar")
        pos + 1
      case _ =>
        val sb = new StringBuilder
        var i  = pos
        while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' && input(
            i
          ) != ';' && input(i) != '"'
        do
          sb += input(i)
          i += 1
        tokens += Token.Atom(sb.toString)
        i

  private def readString(
    input: String,
    start: Int,
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
    tokens += Token.Str(sb.toString)
    i
