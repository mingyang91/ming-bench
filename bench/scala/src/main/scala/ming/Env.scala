package ming

/** Immutable environment with lexical scoping. */
sealed trait Env:
  def lookup(name: String): SchemeValue
  def get(name: String): Option[SchemeValue]

  def extend(name: String, value: SchemeValue): Env =
    Env.Frame(Map(name -> value), this)

  def extend(names: List[String], values: List[SchemeValue]): Env =
    Env.Frame(names.zip(values).toMap, this)

object Env:
  val empty: Env = Frame(Map.empty, null)

  case class Frame(bindings: Map[String, SchemeValue], parent: Env) extends Env:

    def lookup(name: String): SchemeValue =
      bindings.get(name) match
        case Some(v) => v
        case None =>
          if parent == null then throw new EvalError(s"unbound variable: $name")
          else parent.lookup(name)

    def get(name: String): Option[SchemeValue] =
      bindings
        .get(name)
        .orElse(
          if parent == null then None else parent.get(name)
        )

  /** Environment that lazily resolves a recursive binding. */
  class RecursiveFrame(
    val name: String,
    makeLambda: Env => SchemeValue,
    val parent: Env
  ) extends Env:
    lazy val self: SchemeValue = makeLambda(this)

    def lookup(n: String): SchemeValue =
      if n == name then self
      else parent.lookup(n)

    def get(n: String): Option[SchemeValue] =
      if n == name then Some(self)
      else parent.get(n)

  /** Letrec-style frame: all defines see each other via lazy initialization. */
  class LetrecFrame(
    defines: List[(String, List[String], List[SchemeValue])],
    val parent: Env
  ) extends Env:
    import SchemeValue.*

    // Build bindings map lazily — each entry is computed on first access
    private val bindings: Map[String, () => SchemeValue] =
      defines.map { case (name, params, body) =>
        if params.isEmpty then
          // Variable define: (define x expr) — evaluate lazily in this frame
          lazy val value: SchemeValue =
            import ming.Evaluator as E
            E.eval(body.head, this)
          (name, () => value)
        else
          // Function define: create lambda with this frame as closure
          lazy val lambda: SchemeValue = SchemeLambda(params, body, this)
          (name, () => lambda)
      }.toMap

    // Force evaluation of all variable defines
    def init(): Unit =
      defines.foreach { case (name, params, _) =>
        if params.isEmpty then bindings(name)()
      }

    def lookup(n: String): SchemeValue =
      bindings.get(n) match
        case Some(thunk) => thunk()
        case None        => parent.lookup(n)

    def get(n: String): Option[SchemeValue] =
      bindings.get(n) match
        case Some(thunk) => Some(thunk())
        case None        => parent.get(n)
