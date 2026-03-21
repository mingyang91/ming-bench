package ming

enum StringStorage:
  case Immutable(value: String)
  case Mutable(id: Int)
