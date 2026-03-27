package ming;

final class SchemeErrors {
    private SchemeErrors() {
    }

    static EvalError at(SourceLoc loc, String message) {
        return new EvalError(message + " at " + loc.line() + ":" + loc.column());
    }
}
