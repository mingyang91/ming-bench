package ming

/** Source position (1-based line and column). */
case class Pos(line: Int, col: Int):
  override def toString: String = s"$line:$col"

/** Parsed S-expression with optional source position. */
enum Expr:
  case Num(n: Long, pos: Option[Pos] = None)
  case Bool(b: Boolean, pos: Option[Pos] = None)
  case Str(s: String, pos: Option[Pos] = None)
  case Sym(name: String, pos: Option[Pos] = None)
  case Chr(c: Char, pos: Option[Pos] = None)
  case SList(elements: List[Expr], pos: Option[Pos] = None)

  def position: Option[Pos] = this match
    case Num(_, p)   => p
    case Bool(_, p)  => p
    case Str(_, p)   => p
    case Sym(_, p)   => p
    case Chr(_, p)   => p
    case SList(_, p) => p
