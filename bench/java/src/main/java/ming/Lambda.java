package ming;

import java.util.List;

class Lambda {
    final List<String> params;
    final String restParam; // null if not variadic
    final List<Object> body;
    final Env closure;

    Lambda(List<String> params, String restParam, List<Object> body, Env closure) {
        this.params = params;
        this.restParam = restParam;
        this.body = body;
        this.closure = closure;
    }
}
