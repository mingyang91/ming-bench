package ming

private[ming] enum Expr:

  case IntAtom(value: BigInt, pos: SourcePos)
  case RationalAtom(numerator: BigInt, denominator: BigInt, pos: SourcePos)
  case InexactAtom(value: Double, pos: SourcePos)
  case BoolAtom(value: Boolean, pos: SourcePos)
  case StringAtom(value: String, pos: SourcePos)
  case CharAtom(value: Char, pos: SourcePos)
  case Symbol(name: String, pos: SourcePos)
  case ListExpr(items: List[Expr], pos: SourcePos)
