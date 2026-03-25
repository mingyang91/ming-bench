package ming

import Evaluator.{Bounce, Cont, Done, More}

/** Quasiquote expansion — CPS-based. */
object Quasiquote:

  def expandQuasiquoteK(template: SchemeVal, env: Env, k: Cont): Bounce =
    template match
      case SchemeVal.SList(List(SchemeVal.Symbol("unquote"), expr)) =>
        More(() => Evaluator.evalK(expr, env, k))
      case SchemeVal.SList(elems) =>
        expandQQListK(elems, None, env, k)
      case SchemeVal.DottedList(elems, tail) =>
        expandQQListK(elems, Some(tail), env, k)
      case SchemeVal.VectorVal(elems) =>
        expandQQListK(
          elems.toList,
          None,
          env,
          resultList =>
            val items = SchemeVal.toScalaList(resultList)
            k(SchemeVal.VectorVal(items.toArray))
        )
      case _ => k(template)

  /** Expand quasiquote for a list of elements, handling unquote-splicing. */
  private def expandQQListK(elems: List[SchemeVal], dotTail: Option[SchemeVal], env: Env, k: Cont): Bounce =
    def go(remaining: List[SchemeVal], acc: List[SchemeVal]): Bounce =
      remaining match
        case Nil =>
          val resultElems = acc.reverse
          dotTail match
            case None => k(SchemeVal.SList(resultElems))
            case Some(tail) =>
              expandQuasiquoteK(
                tail,
                env,
                tailVal =>
                  if resultElems.isEmpty then k(tailVal)
                  else
                    tailVal match
                      case SchemeVal.SList(Nil)    => k(SchemeVal.SList(resultElems))
                      case SchemeVal.SList(telems) => k(SchemeVal.SList(resultElems ++ telems))
                      case _                       => k(SchemeVal.DottedList(resultElems, tailVal))
              )
        case elem :: rest =>
          isUnquoteSplicing(elem) match
            case Some(expr) =>
              More(() =>
                Evaluator.evalK(
                  expr,
                  env,
                  spliced =>
                    val splicedElems = SchemeVal.toScalaList(spliced)
                    go(rest, splicedElems.reverse ++ acc)
                )
              )
            case None =>
              More(() => expandQuasiquoteK(elem, env, v => go(rest, v :: acc)))
    go(elems, Nil)

  private def isUnquoteSplicing(v: SchemeVal): Option[SchemeVal] = v match
    case SchemeVal.SList(List(SchemeVal.Symbol("unquote-splicing"), expr)) => Some(expr)
    case _                                                                 => None
