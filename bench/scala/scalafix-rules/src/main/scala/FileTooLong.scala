import scalafix.v1._
import scalafix.lint.{Diagnostic, LintSeverity}
import scala.meta._

class FileTooLong extends SyntacticRule("FileTooLong") {
  val maxLines = 1500

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val totalLines = doc.tree.pos.endLine + 1
    if (totalLines > maxLines) {
      Patch.lint(
        new Diagnostic {
          override def message = s"File has $totalLines lines, exceeds maximum of $maxLines"
          override def position = doc.tree.pos
          override def severity = LintSeverity.Warning
        }
      )
    } else Patch.empty
  }
}
