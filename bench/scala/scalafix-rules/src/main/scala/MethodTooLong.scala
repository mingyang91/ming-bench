import scalafix.v1._
import scalafix.lint.{Diagnostic, LintSeverity}
import scala.meta._

class MethodTooLong extends SyntacticRule("MethodTooLong") {
  val maxLines = 100

  override def fix(implicit doc: SyntacticDocument): Patch = {
    doc.tree.collect {
      case defn: Defn.Def =>
        val startLine = defn.body.pos.startLine
        val endLine = defn.body.pos.endLine
        val bodyLines = endLine - startLine + 1
        if (bodyLines > maxLines) {
          Patch.lint(
            new Diagnostic {
              override def message = s"Method '${defn.name.value}' has $bodyLines lines, exceeds maximum of $maxLines"
              override def position = defn.name.pos
              override def severity = LintSeverity.Warning
            }
          )
        } else Patch.empty
    }.asPatch
  }
}
