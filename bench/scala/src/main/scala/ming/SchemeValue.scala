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

  class SchemePair(val cell: Array[SchemeValue]) extends SchemeValue:
    def car: SchemeValue             = cell(0)
    def cdr: SchemeValue             = cell(1)
    def setCar(v: SchemeValue): Unit = cell(0) = v
    def setCdr(v: SchemeValue): Unit = cell(1) = v

    def display: String                = s"(${formatElems(_.display)})"
    override def displayOutput: String = s"(${formatElems(_.displayOutput)})"

    private def formatElems(fmt: SchemeValue => String): String =
      @scala.annotation.tailrec
      def loop(v: SchemeValue, acc: List[String]): List[String] = v match
        case SchemeList(Nil) => acc.reverse
        case SchemeList(es)  => acc.reverse ++ es.map(fmt)
        case p: SchemePair   => loop(p.cdr, fmt(p.car) :: acc)
        case d               => (s". ${fmt(d)}" :: acc).reverse
      loop(cdr, List(fmt(car))).mkString(" ")

  object SchemePair:

    def apply(car: SchemeValue, cdr: SchemeValue): SchemePair =
      new SchemePair(Array(car, cdr))

    def unapply(p: SchemePair): Some[(SchemeValue, SchemeValue)] =
      Some((p.car, p.cdr))

  class SchemeVector(val elements: Array[SchemeValue]) extends SchemeValue:
    def display: String                = s"#(${elements.map(_.display).mkString(" ")})"
    override def displayOutput: String = s"#(${elements.map(_.displayOutput).mkString(" ")})"

  case class SchemeMultipleValues(values: List[SchemeValue]) extends SchemeValue:
    def display: String = values.map(_.display).mkString(", ")

  class SchemeResolvedSymbol(
    val name: String,
    val resolveEnv: Env,
    override val pos: SourcePos = SourcePos.None
  ) extends SchemeValue:
    def display: String = name
