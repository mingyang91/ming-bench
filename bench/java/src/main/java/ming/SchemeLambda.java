package ming;

import java.util.List;

public class SchemeLambda {
    final List<String> params;
    final String restParam; // null if no rest parameter
    final Object body;
    final Environment closure;

    public SchemeLambda(List<String> params, Object body, Environment closure) {
        this(params, null, body, closure);
    }

    public SchemeLambda(List<String> params, String restParam, Object body, Environment closure) {
        this.params = params;
        this.restParam = restParam;
        this.body = body;
        this.closure = closure;
    }
}
