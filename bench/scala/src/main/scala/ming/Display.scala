package ming

import Evaluator.Val
import Evaluator.Val.*

/** Value display/formatting utilities. */
object Display:

  /** Write-style display (with quotes on strings). */
  def write(v: Val): String = v match
    case Num(n)              => n.toString
    case Rational(n, d)      => s"$n/$d"
    case Inexact(d)          => d.toString
    case Bool(true)          => "#t"
    case Bool(false)         => "#f"
    case Str(chars)          => "\"" + new String(chars) + "\""
    case SchemeChar(c)       => s"#\\$c"
    case Symbol(name)        => name
    case Nil                 => "()"
    case Void                => "#<void>"
    case _: Pair             => writeList(v)
    case Builtin(_)          => "#<procedure>"
    case Closure(_, _, _, _) => "#<procedure>"
    case _: ContinuationVal  => "#<continuation>"
    case CallCCVal           => "#<procedure>"
    case MacroTransformer(_) => "#<macro>"
    case Record(tag, _)      => s"#<record:$tag>"
    case Vector(elems)       => "#(" + elems.map(write).mkString(" ") + ")"

  /** Display-style (no quotes on strings). */
  def show(v: Val): String = v match
    case Str(chars)    => new String(chars)
    case SchemeChar(c) => c.toString
    case _: Pair       => showList(v)
    case Vector(elems) => "#(" + elems.map(show).mkString(" ") + ")"
    case _             => write(v)

  /** Write a list/pair with cycle detection. */
  private def writeList(v: Val): String =
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[Pair, java.lang.Boolean]()
    )
    val sb      = new StringBuilder("(")
    var current = v
    var first   = true
    var done    = false
    while !done do
      current match
        case p: Pair =>
          if visited.contains(p) then
            if !first then sb.append(" ")
            sb.append("...")
            done = true
          else
            visited.add(p)
            if !first then sb.append(" ")
            sb.append(write(p.car))
            current = p.cdr
            first = false
        case Nil => done = true
        case _ =>
          sb.append(" . ")
          sb.append(write(current))
          done = true
    sb.append(")")
    sb.toString

  /** Show a list/pair with cycle detection. */
  private def showList(v: Val): String =
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[Pair, java.lang.Boolean]()
    )
    val sb      = new StringBuilder("(")
    var current = v
    var first   = true
    var done    = false
    while !done do
      current match
        case p: Pair =>
          if visited.contains(p) then
            if !first then sb.append(" ")
            sb.append("...")
            done = true
          else
            visited.add(p)
            if !first then sb.append(" ")
            sb.append(show(p.car))
            current = p.cdr
            first = false
        case Nil => done = true
        case _ =>
          sb.append(" . ")
          sb.append(show(current))
          done = true
    sb.append(")")
    sb.toString
