package ming

object PairAccessorBuiltins:

  private def getCar(v: SchemeVal): SchemeVal = v match
    case SchemeVal.PairVal(p)      => p.car
    case SchemeVal.ListVal(h :: _) => h
    case other =>
      throw new EvalError(s"car: expected pair, got ${other.display}")

  private def getCdr(v: SchemeVal): SchemeVal = v match
    case SchemeVal.PairVal(p)      => p.cdr
    case SchemeVal.ListVal(_ :: t) => SchemeVal.ListVal(t)
    case other =>
      throw new EvalError(s"cdr: expected pair, got ${other.display}")

  private def makeCxr(name: String, ops: String): (String, SchemeVal) =
    val fn: SchemeVal => SchemeVal =
      ops.foldRight((v: SchemeVal) => v) { (c, acc) =>
        val op = if c == 'a' then getCar else getCdr
        v => acc(op(v))
      }
    name -> SchemeVal.BuiltinProc(
      name,
      {
        case List(v) => fn(v)
        case a =>
          throw new EvalError(
            s"$name: expected 1 argument, got ${a.length}"
          )
      }
    )

  def all: List[(String, SchemeVal)] =
    val ops = List("a", "d")
    val twoLevel =
      for a <- ops; b <- ops yield (s"c${a}${b}r", s"$a$b")
    val threeLevel =
      for a <- ops; b <- ops; c <- ops
      yield (s"c${a}${b}${c}r", s"$a$b$c")
    val fourLevel =
      for a <- ops; b <- ops; c <- ops; d <- ops
      yield (s"c${a}${b}${c}${d}r", s"$a$b$c$d")
    (twoLevel ++ threeLevel ++ fourLevel).map((name, ops) => makeCxr(name, ops))
