package ming

/** Environment with lexical scoping and mutable cells for set!. */
sealed trait Env:
  def lookup(name: String): SchemeValue
  def get(name: String): Option[SchemeValue]
  def set(name: String, value: SchemeValue): Unit

  def extend(name: String, value: SchemeValue): Env =
    new Env.Frame(Map(name -> Array[SchemeValue](value)), this)

  def extend(names: List[String], values: List[SchemeValue]): Env =
    new Env.Frame(
      names.zip(values.map(v => Array[SchemeValue](v))).toMap,
      this
    )

object Env:
  val empty: Env = new Frame(Map.empty, null)

  class Frame(
    val cells: Map[String, Array[SchemeValue]],
    val parent: Env
  ) extends Env:

    def lookup(name: String): SchemeValue =
      cells.get(name) match
        case Some(cell) => cell(0)
        case None =>
          if parent == null then throw new EvalError(s"unbound variable: $name")
          else parent.lookup(name)

    def get(name: String): Option[SchemeValue] =
      cells
        .get(name)
        .map(_(0))
        .orElse(
          if parent == null then None else parent.get(name)
        )

    def set(name: String, value: SchemeValue): Unit =
      cells.get(name) match
        case Some(cell) => cell(0) = value
        case None =>
          if parent == null then throw new EvalError(s"unbound variable: $name")
          else parent.set(name, value)

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

    def set(n: String, value: SchemeValue): Unit =
      if n == name then throw new EvalError(s"cannot set! recursive binding: $n")
      else parent.set(n, value)

  /** Letrec-style frame: all defines see each other via mutable cells. */
  class LetrecFrame(
    defines: List[(String, Option[List[String]], List[SchemeValue])],
    val parent: Env
  ) extends Env:
    import SchemeValue.*

    private val cells: Map[String, Array[SchemeValue]] =
      defines.map { case (name, _, _) =>
        (name, Array[SchemeValue](SchemeVoid))
      }.toMap

    def init(): Unit =
      // First pass: initialize function defines (no evaluation needed)
      defines.foreach {
        case (name, Some(params), body) =>
          cells(name)(0) = SchemeLambda(params, body, this)
        case _ => ()
      }
      // Second pass: initialize variable defines (may reference functions)
      defines.foreach {
        case (name, None, body) =>
          import ming.Evaluator as E
          cells(name)(0) = E.eval(body.head, this)
        case _ => ()
      }

    def lookup(n: String): SchemeValue =
      cells.get(n) match
        case Some(cell) => cell(0)
        case None       => parent.lookup(n)

    def get(n: String): Option[SchemeValue] =
      cells.get(n).map(_(0)).orElse(parent.get(n))

    def set(n: String, value: SchemeValue): Unit =
      cells.get(n) match
        case Some(cell) => cell(0) = value
        case None       => parent.set(n, value)
