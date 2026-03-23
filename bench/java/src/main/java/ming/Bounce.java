package ming;

import java.util.function.Supplier;

public sealed interface Bounce {
    record Done(SchemeValue value) implements Bounce {}
    record More(Supplier<Bounce> thunk) implements Bounce {}
    record Err(EvalError error) implements Bounce {}
}
