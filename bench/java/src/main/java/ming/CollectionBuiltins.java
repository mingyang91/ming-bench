package ming;

import static ming.EvaluatorSupport.listValue;
import static ming.EvaluatorSupport.requireExactArgs;
import static ming.EvaluatorSupport.requireIndex;
import static ming.EvaluatorSupport.requireProperList;
import static ming.EvaluatorSupport.requireVector;
import static ming.EvaluatorSupport.requireVectorIndex;
import static ming.RuntimeConstants.VOID;

import java.util.ArrayList;
import java.util.List;

final class CollectionBuiltins {
    private CollectionBuiltins() {
    }

    static Value list(List<Value> arguments) {
        return listValue(arguments);
    }

    static Value vector(List<Value> arguments) {
        return new VectorValue(arguments);
    }

    static Value makeVector(List<Value> arguments) throws EvalError {
        if (arguments.size() < 1 || arguments.size() > 2) {
            throw new EvalError("make-vector expected 1 or 2 argument(s)");
        }

        int length = requireIndex(arguments.get(0), "make-vector");
        Value fill = arguments.size() == 2 ? arguments.get(1) : VOID;
        List<Value> elements = new ArrayList<>(length);
        for (int index = 0; index < length; index++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    static Value vectorRef(List<Value> arguments) throws EvalError {
        requireExactArgs("vector-ref", arguments, 2);
        VectorValue vector = requireVector(arguments.get(0), "vector-ref");
        return vector.ref(requireVectorIndex(vector, arguments.get(1), "vector-ref"));
    }

    static Value vectorSet(List<Value> arguments) throws EvalError {
        requireExactArgs("vector-set!", arguments, 3);
        VectorValue vector = requireVector(arguments.get(0), "vector-set!");
        vector.set(requireVectorIndex(vector, arguments.get(1), "vector-set!"), arguments.get(2));
        return VOID;
    }

    static Value vectorLength(List<Value> arguments) throws EvalError {
        requireExactArgs("vector-length", arguments, 1);
        return new IntValue(requireVector(arguments.get(0), "vector-length").length());
    }

    static Value vectorToList(List<Value> arguments) throws EvalError {
        requireExactArgs("vector->list", arguments, 1);
        return listValue(requireVector(arguments.get(0), "vector->list").elements());
    }

    static Value listToVector(List<Value> arguments) throws EvalError {
        requireExactArgs("list->vector", arguments, 1);
        return new VectorValue(requireProperList(arguments.get(0), "list->vector"));
    }
}
