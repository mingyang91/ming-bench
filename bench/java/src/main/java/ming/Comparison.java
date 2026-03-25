package ming;

enum Comparison {
    LESS_THAN("<") {
        @Override
        boolean matches(int value) {
            return value < 0;
        }
    },
    GREATER_THAN(">") {
        @Override
        boolean matches(int value) {
            return value > 0;
        }
    },
    EQUAL("=") {
        @Override
        boolean matches(int value) {
            return value == 0;
        }
    },
    LESS_EQUAL("<=") {
        @Override
        boolean matches(int value) {
            return value <= 0;
        }
    };

    final String name;

    Comparison(String name) {
        this.name = name;
    }

    abstract boolean matches(int value);
}
