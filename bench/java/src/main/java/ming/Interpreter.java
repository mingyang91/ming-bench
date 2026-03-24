package ming;

import java.util.ArrayList;
import java.util.List;

final class Interpreter {
    private final Environment global = new Environment(null);

    Value evalProgram(List<Expr> program) throws EvalError {
        Value last = null;
        for (Expr expr : program) {
            last = eval(expr, global);
        }
        if (last == null) {
            throw EvalError.syntax(new SourcePos(1, 1), "empty program");
        }
        return last;
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        return switch (expr) {
            case Expr.IntegerExpr integerExpr -> new Value.IntegerValue(integerExpr.value());
            case Expr.BooleanExpr booleanExpr -> new Value.BooleanValue(booleanExpr.value());
            case Expr.StringExpr stringExpr -> new Value.StringValue(stringExpr.value());
            case Expr.SymbolExpr symbolExpr -> lookupSymbol(symbolExpr.name(), symbolExpr.pos(), env);
            case Expr.ListExpr listExpr -> evalList(listExpr, env);
        };
    }

    private Value evalList(Expr.ListExpr listExpr, Environment env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw EvalError.syntax(listExpr.pos(), "cannot evaluate empty list");
        }

        Expr operator = elements.get(0);
        if (operator instanceof Expr.SymbolExpr symbolExpr) {
            List<Expr> arguments = elements.subList(1, elements.size());
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, env, symbolExpr.pos());
                case "if" -> evalIf(arguments, env, symbolExpr.pos());
                case "quote" -> evalQuote(arguments, symbolExpr.pos());
                case "lambda" -> evalLambda(arguments, env, symbolExpr.pos());
                case "begin" -> evalBegin(arguments, env);
                case "let" -> evalLet(arguments, env, symbolExpr.pos());
                case "cond" -> evalCond(arguments, env, symbolExpr.pos());
                case "and" -> evalAnd(arguments, env);
                case "or" -> evalOr(arguments, env);
                default -> apply(eval(operator, env), operator, arguments, env);
            };
        }

        return apply(eval(operator, env), operator, elements.subList(1, elements.size()), env);
    }

    private Value evalDefine(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.syntax(pos, "define expects a target and value");
        }

        Expr target = arguments.get(0);
        if (target instanceof Expr.SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw EvalError.syntax(pos, "define variable form expects exactly one value expression");
            }
            Value value = eval(arguments.get(1), env);
            env.define(symbolExpr.name(), value);
            return new Value.VoidValue();
        }

        if (target instanceof Expr.ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty()) {
                throw EvalError.syntax(signatureExpr.pos(), "define function form requires a function name");
            }
            if (!(signature.get(0) instanceof Expr.SymbolExpr nameExpr)) {
                throw EvalError.syntax(signature.get(0).pos(), "function name must be a symbol");
            }

            List<String> parameters = parseParameters(signature.subList(1, signature.size()), signatureExpr.pos());
            List<Expr> body = arguments.subList(1, arguments.size());
            if (body.isEmpty()) {
                throw EvalError.syntax(pos, "define function form requires a body");
            }

            Value.ClosureValue closure = new Value.ClosureValue(parameters, body, env);
            env.define(nameExpr.name(), closure);
            return new Value.VoidValue();
        }

        throw EvalError.syntax(target.pos(), "invalid define target");
    }

    private Value evalIf(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() != 3) {
            throw EvalError.syntax(pos, "if expects exactly 3 arguments");
        }

        Value condition = eval(arguments.get(0), env);
        if (condition.isTruthy()) {
            return eval(arguments.get(1), env);
        }
        return eval(arguments.get(2), env);
    }

    private Value evalQuote(List<Expr> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.syntax(pos, "quote expects exactly 1 argument");
        }
        return quote(arguments.get(0));
    }

    private Value evalLambda(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.syntax(pos, "lambda expects a parameter list and body");
        }
        if (!(arguments.get(0) instanceof Expr.ListExpr parameterList)) {
            throw EvalError.syntax(arguments.get(0).pos(), "lambda parameters must be a list");
        }

        List<String> parameters = parseParameters(parameterList.elements(), parameterList.pos());
        List<Expr> body = arguments.subList(1, arguments.size());
        return new Value.ClosureValue(parameters, body, env);
    }

    private Value evalBegin(List<Expr> arguments, Environment env) throws EvalError {
        return evalSequence(arguments, env);
    }

    private Value evalLet(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.syntax(pos, "let expects bindings and a body");
        }

        if (arguments.get(0) instanceof Expr.SymbolExpr nameExpr) {
            if (arguments.size() < 3) {
                throw EvalError.syntax(pos, "named let expects bindings and a body");
            }
            if (!(arguments.get(1) instanceof Expr.ListExpr bindingsExpr)) {
                throw EvalError.syntax(arguments.get(1).pos(), "named let bindings must be a list");
            }
            return evalNamedLet(nameExpr.name(), bindingsExpr, arguments.subList(2, arguments.size()), env, pos);
        }

        if (!(arguments.get(0) instanceof Expr.ListExpr bindingsExpr)) {
            throw EvalError.syntax(arguments.get(0).pos(), "let bindings must be a list");
        }

        List<Binding> bindings = parseBindings(bindingsExpr.elements());
        List<Value> values = evalBindingValues(bindings, env);
        Environment letEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            letEnv.define(bindings.get(i).name(), values.get(i));
        }
        return evalSequence(arguments.subList(1, arguments.size()), letEnv);
    }

    private Value evalNamedLet(String name, Expr.ListExpr bindingsExpr, List<Expr> body, Environment env, SourcePos pos) throws EvalError {
        List<Binding> bindings = parseBindings(bindingsExpr.elements());
        List<Value> values = evalBindingValues(bindings, env);
        List<String> parameters = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
        }

        Environment namedEnv = new Environment(env);
        Value.ClosureValue closure = new Value.ClosureValue(parameters, body, namedEnv);
        namedEnv.define(name, closure);
        return applyClosure(closure, values, pos);
    }

    private List<Binding> parseBindings(List<Expr> bindingExprs) throws EvalError {
        List<Binding> bindings = new ArrayList<>(bindingExprs.size());
        for (Expr bindingExpr : bindingExprs) {
            if (!(bindingExpr instanceof Expr.ListExpr bindingList)) {
                throw EvalError.syntax(bindingExpr.pos(), "let bindings must be lists");
            }
            if (bindingList.elements().size() != 2) {
                throw EvalError.syntax(bindingList.pos(), "let binding must have exactly 2 elements");
            }
            Expr nameExpr = bindingList.elements().get(0);
            if (!(nameExpr instanceof Expr.SymbolExpr symbolExpr)) {
                throw EvalError.syntax(nameExpr.pos(), "let binding name must be a symbol");
            }
            bindings.add(new Binding(symbolExpr.name(), bindingList.elements().get(1)));
        }
        return List.copyOf(bindings);
    }

    private List<Value> evalBindingValues(List<Binding> bindings, Environment env) throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(eval(binding.valueExpr(), env));
        }
        return values;
    }

    private Value evalCond(List<Expr> arguments, Environment env, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            throw EvalError.syntax(pos, "cond expects at least one clause");
        }

        for (int i = 0; i < arguments.size(); i++) {
            Expr clauseExpr = arguments.get(i);
            if (!(clauseExpr instanceof Expr.ListExpr clause)) {
                throw EvalError.syntax(clauseExpr.pos(), "cond clauses must be lists");
            }
            if (clause.elements().isEmpty()) {
                throw EvalError.syntax(clause.pos(), "cond clause cannot be empty");
            }

            Expr testExpr = clause.elements().get(0);
            List<Expr> body = clause.elements().subList(1, clause.elements().size());
            if (testExpr instanceof Expr.SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (i != arguments.size() - 1) {
                    throw EvalError.syntax(testExpr.pos(), "else clause must be last");
                }
                return evalSequence(body, env);
            }

            Value testValue = eval(testExpr, env);
            if (testValue.isTruthy()) {
                if (body.isEmpty()) {
                    return testValue;
                }
                return evalSequence(body, env);
            }
        }

        return new Value.VoidValue();
    }

    private List<String> parseParameters(List<Expr> parameterExprs, SourcePos pos) throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        for (Expr parameterExpr : parameterExprs) {
            if (!(parameterExpr instanceof Expr.SymbolExpr symbolExpr)) {
                throw EvalError.syntax(parameterExpr.pos(), "parameters must be symbols");
            }
            parameters.add(symbolExpr.name());
        }
        return List.copyOf(parameters);
    }

    private Value quote(Expr expr) throws EvalError {
        return switch (expr) {
            case Expr.IntegerExpr integerExpr -> new Value.IntegerValue(integerExpr.value());
            case Expr.BooleanExpr booleanExpr -> new Value.BooleanValue(booleanExpr.value());
            case Expr.StringExpr stringExpr -> new Value.StringValue(stringExpr.value());
            case Expr.SymbolExpr symbolExpr -> new Value.SymbolValue(symbolExpr.name());
            case Expr.ListExpr listExpr -> {
                List<Value> values = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    values.add(quote(element));
                }
                yield new Value.ListValue(values);
            }
        };
    }

    private Value evalAnd(List<Expr> arguments, Environment env) throws EvalError {
        Value result = new Value.BooleanValue(true);
        for (Expr argument : arguments) {
            result = eval(argument, env);
            if (!result.isTruthy()) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments, Environment env) throws EvalError {
        Value result = new Value.BooleanValue(false);
        for (Expr argument : arguments) {
            result = eval(argument, env);
            if (result.isTruthy()) {
                return result;
            }
        }
        return result;
    }

    private Value lookupSymbol(String name, SourcePos pos, Environment env) throws EvalError {
        Value value = env.lookup(name);
        if (value != null) {
            return value;
        }

        Value builtin = builtinValue(name);
        if (builtin != null) {
            return builtin;
        }

        throw new EvalError(pos, "unbound variable: " + name);
    }

    private Value builtinValue(String name) {
        return switch (name) {
            case "+", "-", "*", "/", "<", ">", "=", "<=", "not",
                    "cons", "car", "cdr", "null?", "list", "length", "append",
                    "string?", "number?", "boolean?", "pair?", "symbol?" ->
                    new Value.BuiltinProcedure(name);
            default -> null;
        };
    }

    private Value apply(Value operator, Expr operatorExpr, List<Expr> argumentExprs, Environment env) throws EvalError {
        return switch (operator) {
            case Value.BuiltinProcedure builtinProcedure -> applyBuiltin(
                    builtinProcedure.name(), argumentExprs, env, operatorExpr.pos());
            case Value.ClosureValue closureValue -> applyClosure(closureValue, argumentExprs, env, operatorExpr.pos());
            default -> throw new EvalError(operatorExpr.pos(), "attempted to call non-procedure");
        };
    }

    private Value applyClosure(Value.ClosureValue closure, List<Expr> argumentExprs, Environment env, SourcePos pos) throws EvalError {
        List<Value> arguments = evalArguments(argumentExprs, env);
        return applyClosure(closure, arguments, pos);
    }

    private Value applyClosure(Value.ClosureValue closure, List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != closure.parameters().size()) {
            throw EvalError.arity(pos, "lambda", "expected " + closure.parameters().size() + " arguments");
        }

        Environment callEnv = new Environment(closure.env());
        for (int i = 0; i < closure.parameters().size(); i++) {
            callEnv.define(closure.parameters().get(i), arguments.get(i));
        }
        return evalSequence(closure.body(), callEnv);
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        Value last = new Value.VoidValue();
        for (Expr expression : expressions) {
            last = eval(expression, env);
        }
        return last;
    }

    private List<Value> evalArguments(List<Expr> argumentExprs, Environment env) throws EvalError {
        List<Value> arguments = new ArrayList<>(argumentExprs.size());
        for (Expr argumentExpr : argumentExprs) {
            arguments.add(eval(argumentExpr, env));
        }
        return arguments;
    }

    private List<EvaluatedArgument> evalArgumentsWithPositions(List<Expr> argumentExprs, Environment env) throws EvalError {
        List<EvaluatedArgument> arguments = new ArrayList<>(argumentExprs.size());
        for (Expr argumentExpr : argumentExprs) {
            arguments.add(new EvaluatedArgument(eval(argumentExpr, env), argumentExpr.pos()));
        }
        return List.copyOf(arguments);
    }

    private Value applyBuiltin(String name, List<Expr> argumentExprs, Environment env, SourcePos pos) throws EvalError {
        List<EvaluatedArgument> arguments = evalArgumentsWithPositions(argumentExprs, env);

        return switch (name) {
            case "+" -> add(arguments, pos);
            case "-" -> subtract(arguments, pos);
            case "*" -> multiply(arguments, pos);
            case "/" -> divide(arguments, pos);
            case "<" -> compare(arguments, pos, "<", (left, right) -> left < right);
            case ">" -> compare(arguments, pos, ">", (left, right) -> left > right);
            case "=" -> compare(arguments, pos, "=", (left, right) -> left == right);
            case "<=" -> compare(arguments, pos, "<=", (left, right) -> left <= right);
            case "not" -> not(arguments, pos);
            case "cons" -> cons(arguments, pos);
            case "car" -> car(arguments, pos);
            case "cdr" -> cdr(arguments, pos);
            case "null?" -> nullPredicate(arguments, pos);
            case "list" -> list(arguments);
            case "length" -> length(arguments, pos);
            case "append" -> append(arguments, pos);
            case "string?" -> predicate("string?", arguments, pos, value -> value instanceof Value.StringValue);
            case "number?" -> predicate("number?", arguments, pos, value -> value instanceof Value.IntegerValue);
            case "boolean?" -> predicate("boolean?", arguments, pos, value -> value instanceof Value.BooleanValue);
            case "pair?" -> predicate("pair?", arguments, pos, value -> value instanceof Value.ListValue listValue && !listValue.elements().isEmpty());
            case "symbol?" -> predicate("symbol?", arguments, pos, value -> value instanceof Value.SymbolValue);
            default -> throw new EvalError(pos, "unknown procedure: " + name);
        };
    }

    private Value add(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        long total = 0;
        for (EvaluatedArgument argument : arguments) {
            total += expectInteger(argument, "+");
        }
        return new Value.IntegerValue(total);
    }

    private Value subtract(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            throw EvalError.arity(pos, "-", "expected at least 1 argument");
        }

        long result = expectInteger(arguments.get(0), "-");
        if (arguments.size() == 1) {
            return new Value.IntegerValue(-result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result -= expectInteger(arguments.get(i), "-");
        }
        return new Value.IntegerValue(result);
    }

    private Value multiply(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        long product = 1;
        for (EvaluatedArgument argument : arguments) {
            product *= expectInteger(argument, "*");
        }
        return new Value.IntegerValue(product);
    }

    private Value divide(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw EvalError.arity(pos, "/", "expected at least 2 arguments");
        }

        long result = expectInteger(arguments.get(0), "/");
        for (int i = 1; i < arguments.size(); i++) {
            EvaluatedArgument argument = arguments.get(i);
            long divisor = expectInteger(argument, "/");
            if (divisor == 0) {
                throw new EvalError(argument.pos(), "division by zero");
            }
            result /= divisor;
        }
        return new Value.IntegerValue(result);
    }

    private Value compare(List<EvaluatedArgument> arguments, SourcePos pos, String name, LongComparison comparison)
            throws EvalError {
        if (arguments.size() <= 1) {
            return new Value.BooleanValue(true);
        }

        long previous = expectInteger(arguments.get(0), name);
        for (int i = 1; i < arguments.size(); i++) {
            long current = expectInteger(arguments.get(i), name);
            if (!comparison.test(previous, current)) {
                return new Value.BooleanValue(false);
            }
            previous = current;
        }
        return new Value.BooleanValue(true);
    }

    private Value not(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, "not", "expected exactly 1 argument");
        }
        return new Value.BooleanValue(!arguments.get(0).value().isTruthy());
    }

    private Value cons(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 2) {
            throw EvalError.arity(pos, "cons", "expected exactly 2 arguments");
        }

        Value tail = arguments.get(1).value();
        if (!(tail instanceof Value.ListValue listValue)) {
            throw EvalError.type(arguments.get(1).pos(), "cons expects a list as its second argument");
        }

        List<Value> elements = new ArrayList<>(listValue.elements().size() + 1);
        elements.add(arguments.get(0).value());
        elements.addAll(listValue.elements());
        return new Value.ListValue(elements);
    }

    private Value car(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, "car", "expected exactly 1 argument");
        }

        Value.ListValue listValue = expectNonEmptyList(arguments.get(0), "car");
        return listValue.elements().get(0);
    }

    private Value cdr(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, "cdr", "expected exactly 1 argument");
        }

        Value.ListValue listValue = expectNonEmptyList(arguments.get(0), "cdr");
        return new Value.ListValue(listValue.elements().subList(1, listValue.elements().size()));
    }

    private Value nullPredicate(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, "null?", "expected exactly 1 argument");
        }
        return new Value.BooleanValue(
                arguments.get(0).value() instanceof Value.ListValue listValue && listValue.elements().isEmpty());
    }

    private Value list(List<EvaluatedArgument> arguments) {
        List<Value> values = new ArrayList<>(arguments.size());
        for (EvaluatedArgument argument : arguments) {
            values.add(argument.value());
        }
        return new Value.ListValue(values);
    }

    private Value length(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, "length", "expected exactly 1 argument");
        }

        Value.ListValue listValue = expectList(arguments.get(0), "length");
        return new Value.IntegerValue(listValue.elements().size());
    }

    private Value append(List<EvaluatedArgument> arguments, SourcePos pos) throws EvalError {
        List<Value> combined = new ArrayList<>();
        for (EvaluatedArgument argument : arguments) {
            Value.ListValue listValue = expectList(argument, "append");
            combined.addAll(listValue.elements());
        }
        return new Value.ListValue(combined);
    }

    private Value predicate(String name, List<EvaluatedArgument> arguments, SourcePos pos, ValuePredicate predicate)
            throws EvalError {
        if (arguments.size() != 1) {
            throw EvalError.arity(pos, name, "expected exactly 1 argument");
        }
        return new Value.BooleanValue(predicate.test(arguments.get(0).value()));
    }

    private Value.ListValue expectList(EvaluatedArgument argument, String name) throws EvalError {
        if (argument.value() instanceof Value.ListValue listValue) {
            return listValue;
        }
        throw EvalError.type(argument.pos(), name + " expects list arguments");
    }

    private Value.ListValue expectNonEmptyList(EvaluatedArgument argument, String name) throws EvalError {
        Value.ListValue listValue = expectList(argument, name);
        if (listValue.elements().isEmpty()) {
            throw EvalError.type(argument.pos(), name + " expects a non-empty list");
        }
        return listValue;
    }

    private long expectInteger(EvaluatedArgument argument, String name) throws EvalError {
        if (argument.value() instanceof Value.IntegerValue integerValue) {
            return integerValue.value();
        }
        throw EvalError.type(argument.pos(), name + " expects integer arguments");
    }

    private record Binding(String name, Expr valueExpr) {}

    private record EvaluatedArgument(Value value, SourcePos pos) {}

    @FunctionalInterface
    private interface LongComparison {
        boolean test(long left, long right);
    }

    @FunctionalInterface
    private interface ValuePredicate {
        boolean test(Value value);
    }
}
