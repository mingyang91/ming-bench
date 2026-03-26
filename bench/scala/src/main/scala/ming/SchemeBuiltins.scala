package ming

import SchemeModel.*

private[ming] object SchemeBuiltins:

  def bindings(output: StringBuilder): List[(String, Value)] =
    SchemeNumericBuiltins.bindings ++
      SchemeCoreBuiltins.bindings ++
      SchemeVectorBuiltins.bindings ++
      SchemeOutputBuiltins.bindings(output) ++
      SchemeTextBuiltins.bindings ++
      SchemePredicateBuiltins.bindings
