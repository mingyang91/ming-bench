package ming

/** Mutable pair cell for cons pairs. Mutability via array indexing (no class-level var). */
final class PairCell(private val data: Array[Value]):
  def car: Value             = data(0)
  def cdr: Value             = data(1)
  def setCar(v: Value): Unit = data(0) = v
  def setCdr(v: Value): Unit = data(1) = v

object PairCell:
  def apply(car: Value, cdr: Value): PairCell    = new PairCell(Array(car, cdr))
  def unapply(c: PairCell): Some[(Value, Value)] = Some((c.car, c.cdr))

/** Convenience extractor/factory for Value.PairVal — drop-in replacement for old PairVal(car, cdr). */
object Pair:
  def apply(car: Value, cdr: Value): Value = Value.PairVal(PairCell(car, cdr))

  def unapply(v: Value): Option[(Value, Value)] = v match
    case Value.PairVal(cell) => Some((cell.car, cell.cdr))
    case _                   => None
