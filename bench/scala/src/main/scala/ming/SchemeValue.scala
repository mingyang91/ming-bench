package ming

/** Scheme value representation. */
sealed trait SchemeValue:
  def display: String
  def displayOutput: String = display
  def pos: SourcePos        = SourcePos.None

object SchemeValue:

  case class SchemeInt(value: Long) extends SchemeValue:
    def display: String = value.toString

  case class SchemeBool(value: Boolean) extends SchemeValue:
    def display: String = if value then "#t" else "#f"

  case class SchemeString(value: String) extends SchemeValue:
    def display: String                = "\"" + value + "\""
    override def displayOutput: String = value

  class SchemeMutableString(val chars: Array[Char]) extends SchemeValue:
    def display: String                = "\"" + String(chars) + "\""
    override def displayOutput: String = String(chars)

  class SchemeSymbol(val name: String, override val pos: SourcePos = SourcePos.None) extends SchemeValue:
    def display: String = name

    override def equals(other: Any): Boolean = other match
      case s: SchemeSymbol => s.name == name
      case _               => false
    override def hashCode: Int    = name.hashCode
    override def toString: String = s"SchemeSymbol($name)"

  object SchemeSymbol:

    def apply(name: String, pos: SourcePos = SourcePos.None): SchemeSymbol =
      new SchemeSymbol(name, pos)

    def unapply(v: SchemeValue): Option[String] = v match
      case s: SchemeSymbol => Some(s.name)
      case _               => scala.None

  class SchemeList(val elements: List[SchemeValue], override val pos: SourcePos = SourcePos.None) extends SchemeValue:
    def display: String                = s"(${elements.map(_.display).mkString(" ")})"
    override def displayOutput: String = s"(${elements.map(_.displayOutput).mkString(" ")})"

    override def equals(other: Any): Boolean = other match
      case l: SchemeList => l.elements == elements
      case _             => false
    override def hashCode: Int    = elements.hashCode
    override def toString: String = s"SchemeList(${elements.mkString(", ")})"

  object SchemeList:

    def apply(elements: List[SchemeValue], pos: SourcePos = SourcePos.None): SchemeList =
      new SchemeList(elements, pos)

    def unapply(v: SchemeValue): Option[List[SchemeValue]] = v match
      case l: SchemeList => Some(l.elements)
      case _             => scala.None

  case class SchemeLambda(
    params: List[String],
    restParam: Option[String],
    body: List[SchemeValue],
    closure: Env
  ) extends SchemeValue:
    def display: String = "#<procedure>"

  case class SchemeBuiltinProc(name: String) extends SchemeValue:
    def display: String = s"#<procedure:$name>"

  case class SchemeChar(value: Char) extends SchemeValue:
    def display: String = value match
      case ' '  => "#\\space"
      case '\n' => "#\\newline"
      case '\t' => "#\\tab"
      case c    => s"#\\$c"
    override def displayOutput: String = value.toString

  case class SchemeContinuation(k: Kont) extends SchemeValue:
    def display: String = "#<continuation>"

  case object SchemeVoid extends SchemeValue:
    def display: String = "#<void>"

  case class SchemeMacro(
    literals: List[String],
    rules: List[(SchemeValue, SchemeValue)],
    defEnv: Env
  ) extends SchemeValue:
    def display: String = "#<macro>"

  case class SchemePair(car: SchemeValue, cdr: SchemeValue) extends SchemeValue:
    def display: String =
      val carStr = car.display
      cdr match
        case SchemeList(Nil) => s"($carStr)"
        case SchemeList(es)  => s"($carStr ${es.map(_.display).mkString(" ")})"
        case _: SchemePair   => s"($carStr ${pairTail(cdr)})"
        case _               => s"($carStr . ${cdr.display})"

    private def pairTail(v: SchemeValue): String = v match
      case SchemePair(a, SchemeList(Nil)) => a.display
      case SchemePair(a, d: SchemePair)   => s"${a.display} ${pairTail(d)}"
      case SchemePair(a, d)               => s"${a.display} . ${d.display}"
      case _                               => s". ${v.display}"

  class SchemeResolvedSymbol(
    val name: String,
    val resolveEnv: Env,
    override val pos: SourcePos = SourcePos.None
  ) extends SchemeValue:
    def display: String = name
