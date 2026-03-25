package ming

private[ming] object PairOps:

  val NIL: Expr = Expr.Lst(Nil)

  def cons(car: Expr, cdr: Expr): Expr =
    Expr.Pair(new MutablePair(car, cdr))

  def makeList(elems: List[Expr]): Expr =
    elems.foldRight(NIL)(cons)

  def carOf(e: Expr): Expr = e match
    case Expr.Pair(cell)  => cell.car
    case Expr.Lst(h :: _) => h
    case _                => throw EvalError("car: not a pair")

  def cdrOf(e: Expr): Expr = e match
    case Expr.Pair(cell)  => cell.cdr
    case Expr.Lst(_ :: t) => Expr.Lst(t)
    case _                => throw EvalError("cdr: not a pair")

  def isPair(e: Expr): Boolean = e match
    case Expr.Pair(_)     => true
    case Expr.Lst(_ :: _) => true
    case _                => false

  def isNull(e: Expr): Boolean = e match
    case Expr.Lst(Nil) => true
    case _             => false

  def toScalaList(e: Expr): List[Expr] =
    val buf   = scala.collection.mutable.ListBuffer[Expr]()
    var cur   = e
    var steps = 0
    while true do
      cur match
        case Expr.Lst(Nil)   => return buf.toList
        case Expr.Lst(elems) => return (buf ++= elems).toList
        case Expr.Pair(cell) =>
          buf += cell.car
          cur = cell.cdr
          steps += 1
          if steps > 10000000 then throw EvalError("not a proper list: too long or circular")
        case _ => throw EvalError(s"not a proper list: ${Display.display(e)}")
    buf.toList // unreachable

  def toScalaListSafe(e: Expr): Option[List[Expr]] =
    val buf = scala.collection.mutable.ListBuffer[Expr]()
    var cur = e
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[MutablePair, java.lang.Boolean]()
    )
    while true do
      cur match
        case Expr.Lst(Nil)   => return Some(buf.toList)
        case Expr.Lst(elems) => return Some((buf ++= elems).toList)
        case Expr.Pair(cell) =>
          if !visited.add(cell) then return None // cycle
          buf += cell.car
          cur = cell.cdr
        case _ => return None
    None // unreachable

  def isList(e: Expr): Boolean = e match
    case Expr.Lst(_)  => true
    case Expr.Pair(_) =>
      // Floyd's tortoise and hare cycle detection
      var slow = e
      var fast = e
      while true do
        // advance fast by 1
        fast match
          case Expr.Pair(c) => fast = c.cdr
          case Expr.Lst(_)  => return true
          case _            => return false
        // check meeting
        if fast eq slow then return false
        // advance fast by 1 more
        fast match
          case Expr.Pair(c) => fast = c.cdr
          case Expr.Lst(_)  => return true
          case _            => return false
        // advance slow by 1
        slow match
          case Expr.Pair(c) => slow = c.cdr
          case _            => return true // slow reached Lst end
        // check meeting
        if fast eq slow then return false
      false
    case _ => false

  def lengthOf(e: Expr): Long =
    var count = 0L
    var cur   = e
    while true do
      cur match
        case Expr.Lst(Nil)   => return count
        case Expr.Lst(elems) => return count + elems.length
        case Expr.Pair(cell) =>
          count += 1
          cur = cell.cdr
        case _ => throw EvalError("length: not a proper list")
    count // unreachable
