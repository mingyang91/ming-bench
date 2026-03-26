package ming;

import java.util.List;

class Lambda {
    final List<String> params;
    final List<Object> body;
    final Env closure;

    Lambda(List<String> params, List<Object> body, Env closure) {
        this.params = params;
        this.body = body;
        this.closure = closure;
    }
}
