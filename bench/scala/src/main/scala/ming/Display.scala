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
    case Pair(_, _)          => writeList(v)
    case Builtin(_)          => "#<procedure>"
    case MacroTransformer(_) => "#<macro>"
    case Record(tag, _)      => s"#<record:$tag>"

  /** Display-style (no quotes on strings). */
  def show(v: Val): String = v match
    case Str(chars)    => new String(chars)
    case SchemeChar(c) => c.toString
    case Pair(_, _)    => showList(v)
    case _             => write(v)

  private def writeList(v: Val): String =
    val sb = new StringBuilder("(")
    @scala.annotation.tailrec
    def loop(current: Val, first: Boolean): Unit = current match
      case Pair(car, cdr) =>
        if !first then sb.append(" ")
        sb.append(write(car))
        loop(cdr, first = false)
      case Nil => ()
      case _   => sb.append(" . "); sb.append(write(current))
    loop(v, first = true)
    sb.append(")")
    sb.toString

  private def showList(v: Val): String =
    val sb = new StringBuilder("(")
    @scala.annotation.tailrec
    def loop(current: Val, first: Boolean): Unit = current match
      case Pair(car, cdr) =>
        if !first then sb.append(" ")
        sb.append(show(car))
        loop(cdr, first = false)
      case Nil => ()
      case _   => sb.append(" . "); sb.append(show(current))
    loop(v, first = true)
    sb.append(")")
    sb.toString
