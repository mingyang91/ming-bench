package ming

import Evaluator.*
import Evaluator.Val.*

/** Quasiquote expansion — handles `quasiquote`, `unquote`, and `unquote-splicing`. */
object Quasiquote:

  def evalQuasiquoteK(template: Val, env: Env, k: Cont): Bounce =
    template match
      case Pair(Symbol("unquote"), Pair(expr, Nil)) =>
        evalK(expr, env, k)
      case Pair(Symbol("quasiquote"), _) =>
        // Nested quasiquote — return as-is (don't expand inner level)
        k(template)
      case p: Pair =>
        // Check for (unquote-splicing ...) in car position
        p.car match
          case Pair(Symbol("unquote-splicing"), Pair(expr, Nil)) =>
            evalK(
              expr,
              env,
              spliced =>
                evalQuasiquoteK(
                  p.cdr,
                  env,
                  restResult => k(appendScheme(spliced, restResult))
                )
            )
          case _ =>
            evalQuasiquoteK(
              p.car,
              env,
              carResult =>
                evalQuasiquoteK(
                  p.cdr,
                  env,
                  cdrResult => k(Pair(carResult, cdrResult))
                )
            )
      case v: Vector =>
        evalQuasiquoteVecK(v.elems.toList, env, results => k(Vector(results.toArray)))
      case _ => k(template)

  private def evalQuasiquoteVecK(
    elems: List[Val],
    env: Env,
    k: List[Val] => Bounce
  ): Bounce =
    elems match
      case scala.Nil => k(scala.Nil)
      case head :: tail =>
        head match
          case Pair(Symbol("unquote-splicing"), Pair(expr, Nil)) =>
            evalK(
              expr,
              env,
              spliced =>
                evalQuasiquoteVecK(
                  tail,
                  env,
                  restResults => k(toList(spliced) ++ restResults)
                )
            )
          case _ =>
            evalQuasiquoteK(
              head,
              env,
              headResult => evalQuasiquoteVecK(tail, env, tailResults => k(headResult :: tailResults))
            )

  private def appendScheme(a: Val, b: Val): Val = a match
    case Nil            => b
    case Pair(car, cdr) => Pair(car, appendScheme(cdr, b))
    case _              => error(s"unquote-splicing: not a proper list: ${Display.write(a)}")
