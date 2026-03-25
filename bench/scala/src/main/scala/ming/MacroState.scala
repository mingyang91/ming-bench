package ming

import scala.collection.mutable

final private[ming] class MacroState:

  private val macros = mutable.LinkedHashMap.empty[String, SyntaxMacro]
  private var nextId = 0

  def define(name: String, macroDef: SyntaxMacro): Unit =
    macros.update(name, macroDef)

  def lookup(name: String): Option[SyntaxMacro] =
    macros.get(name)

  def isMacro(name: String): Boolean =
    macros.contains(name)

  def fresh(base: String): String =
    nextId += 1
    val suffix =
      base.map(ch => if ch.isLetterOrDigit then ch else '_').mkString match
        case ""   => "id"
        case text => text

    s"__macro_${nextId}_$suffix"

private[ming] object MacroState:

  def apply(): MacroState =
    new MacroState

final private[ming] case class SyntaxMacro(
  literals: Set[String],
  rules: List[SyntaxRule],
  definitionEnv: Env,
  aliases: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap.empty
)

final private[ming] case class SyntaxRule(pattern: Expr, template: Expr)
