package ming

enum Bounce:
  case Done(value: SchemeValue, env: Map[String, SchemeValue])
  case More(thunk: () => Bounce)
