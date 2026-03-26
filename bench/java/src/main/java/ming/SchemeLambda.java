package ming;

import java.util.List;

public class SchemeLambda {
    final List<String> params;
    final Object body;
    final Environment closure;

    public SchemeLambda(List<String> params, Object body, Environment closure) {
        this.params = params;
        this.body = body;
        this.closure = closure;
    }
}
