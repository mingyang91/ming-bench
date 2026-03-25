package ming

import Evaluator.{Bounce, Cont, Done, More}

/** Dynamic-wind winder state and wind/unwind operations. */
object DynamicWind:

  case class Winder(inThunk: SchemeVal, outThunk: SchemeVal)

  def evalDynamicWindK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size != 3 then throw new EvalError("dynamic-wind: expected 3 arguments")
    Evaluator.evalK(
      args(0),
      env,
      inThunk =>
        Evaluator.evalK(
          args(1),
          env,
          bodyThunk =>
            Evaluator.evalK(
              args(2),
              env,
              outThunk =>
                More(() =>
                  Evaluator.applyK(
                    inThunk,
                    Nil,
                    _ =>
                      val w = Winder(inThunk, outThunk)
                      Evaluator.winders = w :: Evaluator.winders
                      More(() =>
                        Evaluator.applyK(
                          bodyThunk,
                          Nil,
                          bodyVal =>
                            Evaluator.winders = Evaluator.winders.tail
                            More(() => Evaluator.applyK(outThunk, Nil, _ => k(bodyVal)))
                        )
                      )
                  )
                )
            )
        )
    )

  /** Wind/unwind between current and target winder stacks, then call k. */
  def doWindK(from: List[Winder], to: List[Winder], k: () => Bounce): Bounce =
    val fromLen = from.length
    val toLen   = to.length
    var f       = from
    var t       = to
    if fromLen > toLen then f = f.drop(fromLen - toLen)
    else if toLen > fromLen then t = t.drop(toLen - fromLen)
    while f.nonEmpty && t.nonEmpty && !(f.head eq t.head) do
      f = f.tail
      t = t.tail
    val common   = f
    val toUnwind = from.take(from.length - common.length)
    val toRewind = to.take(to.length - common.length).reverse
    unwindK(toUnwind, () => rewindK(toRewind, k))

  private def unwindK(ws: List[Winder], k: () => Bounce): Bounce =
    ws match
      case Nil => k()
      case w :: rest =>
        Evaluator.winders = Evaluator.winders.tail
        More(() => Evaluator.applyK(w.outThunk, Nil, _ => unwindK(rest, k)))

  private def rewindK(ws: List[Winder], k: () => Bounce): Bounce =
    ws match
      case Nil => k()
      case w :: rest =>
        More(() =>
          Evaluator.applyK(
            w.inThunk,
            Nil,
            _ =>
              Evaluator.winders = w :: Evaluator.winders
              rewindK(rest, k)
          )
        )
