package ming;

import java.util.List;

public class Lambda {
    final List<String> params;
    final String restParam; // null if no rest parameter
    final List<Object> body;
    final Environment closure;

    public Lambda(List<String> params, String restParam, List<Object> body, Environment closure) {
        this.params = params;
        this.restParam = restParam;
        this.body = body;
        this.closure = closure;
    }
}
