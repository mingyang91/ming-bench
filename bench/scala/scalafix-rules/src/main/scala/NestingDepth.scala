import scalafix.v1._
import scalafix.lint.{Diagnostic, LintSeverity}
import scala.meta._

class NestingDepth extends SyntacticRule("NestingDepth") {
  val maxDepth = 6

  override def fix(implicit doc: SyntacticDocument): Patch = {
    def checkNesting(tree: Tree, currentDepth: Int): List[Patch] = {
      val isNesting: Boolean = tree match {
        case _: Term.If       => true
        case _: Term.Match    => true
        case _: Term.While    => true
        case _: Term.For      => true
        case _: Term.ForYield => true
        case _: Term.Try      => true
        case _                => false
      }

      val newDepth = if (isNesting) currentDepth + 1 else currentDepth
      if (newDepth > maxDepth && isNesting) {
        List(Patch.lint(
          new Diagnostic {
            override def message = s"Nesting depth $newDepth exceeds maximum of $maxDepth"
            override def position = tree.pos
            override def severity = LintSeverity.Warning
          }
        ))
      } else {
        tree.children.flatMap(child => checkNesting(child, newDepth)).toList
      }
    }

    doc.tree.collect {
      case defn: Defn.Def => checkNesting(defn.body, 0)
    }.flatten.asPatch
  }
}
