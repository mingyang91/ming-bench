package ming

import Evaluator.*
import Evaluator.Val.*

/** CPS binding form handlers: let, let*, letrec, letrec*, do. */
private[ming] object BindingForms:

  def evalLetK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(Symbol(name), Pair(bindings, body)) =>
        evalNamedLetK(name, bindings, body, env, k)
      case Pair(bindings, body) =>
        evalRegularLetK(bindings, body, env, k)
      case _ => Evaluator.error("bad let syntax")

  private def evalRegularLetK(bindings: Val, body: Val, env: Env, k: Cont): Bounce =
    val childEnv    = Env.empty(Some(env))
    val bindingList = Evaluator.toList(bindings)
    def loopBindings(remaining: List[Val]): Bounce =
      remaining match
        case scala.Nil =>
          val bodyList = Evaluator.toList(body)
          if bodyList.isEmpty then Evaluator.error("let: empty body")
          Evaluator.evalSeqK(bodyList, childEnv, k)
        case Pair(Symbol(name), Pair(valueExpr, Nil)) :: rest =>
          Evaluator.evalK(
            valueExpr,
            env,
            v =>
              childEnv.define(name, v)
              BMore(() => loopBindings(rest))
          )
        case _ => Evaluator.error("bad let binding")
    loopBindings(bindingList)

  private def evalNamedLetK(
    name: String,
    bindings: Val,
    body: Val,
    env: Env,
    k: Cont
  ): Bounce =
    val bindingList = Evaluator.toList(bindings)
    val paramNames = bindingList.map {
      case Pair(Symbol(p), Pair(_, Nil)) => p
      case _                             => Evaluator.error("bad named let binding")
    }
    def evalInits(remaining: List[Val], acc: List[Val]): Bounce =
      remaining match
        case scala.Nil =>
          val initVals = acc.reverse
          val bodyList = Evaluator.toList(body)
          if bodyList.isEmpty then Evaluator.error("let: empty body")
          val closureEnvForName = Env.empty(Some(env))
          val closure           = Closure(paramNames, None, bodyList, closureEnvForName)
          closureEnvForName.define(name, closure)
          val callEnv = Env.empty(Some(closureEnvForName))
          paramNames.zip(initVals).foreach((p, a) => callEnv.define(p, a))
          Evaluator.evalSeqK(bodyList, callEnv, k)
        case Pair(_, Pair(v, Nil)) :: rest =>
          Evaluator.evalK(v, env, initVal => BMore(() => evalInits(rest, initVal :: acc)))
        case _ => Evaluator.error("bad named let binding")
    evalInits(bindingList, scala.Nil)

  def evalLetStarK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(bindings, body) =>
        val childEnv    = Env.empty(Some(env))
        val bindingList = Evaluator.toList(bindings)
        def loopBindings(remaining: List[Val]): Bounce =
          remaining match
            case scala.Nil =>
              val bodyList = Evaluator.toList(body)
              if bodyList.isEmpty then Evaluator.error("let*: empty body")
              Evaluator.evalSeqK(bodyList, childEnv, k)
            case Pair(Symbol(name), Pair(valueExpr, Nil)) :: rest =>
              Evaluator.evalK(
                valueExpr,
                childEnv,
                v =>
                  childEnv.define(name, v)
                  BMore(() => loopBindings(rest))
              )
            case _ => Evaluator.error("bad let* binding")
        loopBindings(bindingList)
      case _ => Evaluator.error("bad let* syntax")

  def evalLetrecK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(bindings, body) =>
        val childEnv    = Env.empty(Some(env))
        val bindingList = Evaluator.toList(bindings)
        val names = bindingList.map {
          case Pair(Symbol(name), Pair(_, Nil)) => name
          case _                                => Evaluator.error("bad letrec binding")
        }
        names.foreach(n => childEnv.define(n, Void))
        def loopBindings(remaining: List[Val]): Bounce =
          remaining match
            case scala.Nil =>
              val bodyList = Evaluator.toList(body)
              if bodyList.isEmpty then Evaluator.error("letrec: empty body")
              Evaluator.evalSeqK(bodyList, childEnv, k)
            case Pair(Symbol(name), Pair(valueExpr, Nil)) :: rest =>
              Evaluator.evalK(
                valueExpr,
                childEnv,
                v =>
                  childEnv.define(name, v)
                  BMore(() => loopBindings(rest))
              )
            case _ => Evaluator.error("bad letrec binding")
        loopBindings(bindingList)
      case _ => Evaluator.error("bad letrec syntax")

  def evalLetrecStarK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(bindings, body) =>
        val childEnv = Env.empty(Some(env))
        def loopBindings(remaining: List[Val]): Bounce =
          remaining match
            case scala.Nil =>
              val bodyList = Evaluator.toList(body)
              if bodyList.isEmpty then Evaluator.error("letrec*: empty body")
              Evaluator.evalSeqK(bodyList, childEnv, k)
            case Pair(Symbol(name), Pair(valueExpr, Nil)) :: rest =>
              Evaluator.evalK(
                valueExpr,
                childEnv,
                v =>
                  childEnv.define(name, v)
                  BMore(() => loopBindings(rest))
              )
            case _ => Evaluator.error("bad letrec* binding")
        loopBindings(Evaluator.toList(bindings))
      case _ => Evaluator.error("bad letrec* syntax")

  def evalDoK(rest: Val, env: Env, k: Cont): Bounce =
    rest match
      case Pair(varSpecs, Pair(testClause, body)) =>
        val specs = Evaluator.toList(varSpecs).map { spec =>
          Evaluator.toList(spec) match
            case List(Symbol(name), init)       => (name, init, None: Option[Val])
            case List(Symbol(name), init, step) => (name, init, Some(step))
            case _                              => Evaluator.error("bad do variable spec")
        }
        val testList = Evaluator.toList(testClause)
        if testList.isEmpty then Evaluator.error("bad do test clause")
        val testExpr    = testList.head
        val resultExprs = testList.tail
        val bodyExprs   = Evaluator.toList(body)
        val doEnv       = Env.empty(Some(env))

        def evalInits(remaining: List[(String, Val, Option[Val])], acc: List[Val]): Bounce =
          remaining match
            case scala.Nil =>
              val initVals = acc.reverse
              specs.zip(initVals).foreach { case ((name, _, _), v) => doEnv.define(name, v) }
              doLoop()
            case (_, init, _) :: rest =>
              Evaluator.evalK(init, env, v => BMore(() => evalInits(rest, v :: acc)))

        def doLoop(): Bounce =
          Evaluator.evalK(
            testExpr,
            doEnv,
            testVal =>
              if testVal != Bool(false) then
                if resultExprs.isEmpty then k(Void)
                else Evaluator.evalSeqK(resultExprs, doEnv, k)
              else
                Evaluator.evalSeqK(
                  bodyExprs,
                  doEnv,
                  _ => evalSteps(specs, scala.Nil)
                )
          )

        def evalSteps(
          remaining: List[(String, Val, Option[Val])],
          acc: List[Val]
        ): Bounce =
          remaining match
            case scala.Nil =>
              val newVals = acc.reverse
              specs.zip(newVals).foreach { case ((name, _, _), v) =>
                doEnv.define(name, v)
              }
              BMore(() => doLoop())
            case (name, _, step) :: rest =>
              step match
                case Some(stepExpr) =>
                  Evaluator.evalK(stepExpr, doEnv, v => BMore(() => evalSteps(rest, v :: acc)))
                case None =>
                  val v = doEnv.lookup(name).getOrElse(Evaluator.error(s"unbound: $name"))
                  evalSteps(rest, v :: acc)

        evalInits(specs, scala.Nil)
      case _ => Evaluator.error("bad do syntax")
