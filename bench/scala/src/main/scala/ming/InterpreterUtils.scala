package ming

import SchemeValue.*

object InterpreterUtils:

  def fmtPos(pos: Option[(Int, Int)]): String =
    pos.map { case (l, c) => s" [$l:$c]" }.getOrElse("")

  def hasPos(msg: String): Boolean =
    msg.matches(".*\\d+:\\d+.*")

  def extractParams(params: List[SchemeValue]): List[String] =
    params.map {
      case SymbolVal(n, _) => n
      case other =>
        throw new EvalError(s"invalid parameter: ${other.display}")
    }

  def isDefine(expr: SchemeValue): Boolean = expr match
    case ListVal(SymbolVal("define", _) :: _, _) => true
    case _                                       => false

  def makeCell(v: SchemeValue): Cell = Cell(Array(v))

  def deref(v: SchemeValue): SchemeValue = v match
    case Cell(arr) => arr(0)
    case other     => other

  def extractDefineName(expr: SchemeValue): String = expr match
    case ListVal(SymbolVal("define", _) :: SymbolVal(name, _) :: _, _) => name
    case ListVal(
          SymbolVal("define", _) :: ListVal(SymbolVal(name, _) :: _, _) :: _,
          _
        ) =>
      name
    case _ => throw new EvalError("invalid define")
