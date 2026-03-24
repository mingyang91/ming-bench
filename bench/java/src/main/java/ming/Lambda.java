package ming;

import java.util.List;

public class Lambda {
    final List<String> params;
    final List<Object> body;
    final Environment closure;

    public Lambda(List<String> params, List<Object> body, Environment closure) {
        this.params = params;
        this.body = body;
        this.closure = closure;
    }
}
