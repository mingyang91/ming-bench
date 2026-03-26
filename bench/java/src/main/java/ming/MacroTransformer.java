package ming;

/**
 * Wraps a procedure (lambda) as a macro transformer for syntax-case macros.
 */
class MacroTransformer {
    final Object procedure;

    MacroTransformer(Object procedure) {
        this.procedure = procedure;
    }
}
