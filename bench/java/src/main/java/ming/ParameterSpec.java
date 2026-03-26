package ming;

import java.util.List;

record ParameterSpec(List<String> fixedParameters, String restParameter) {
    ParameterSpec {
        fixedParameters = List.copyOf(fixedParameters);
    }

    boolean matchesArity(int argumentCount) {
        if (restParameter == null) {
            return argumentCount == fixedParameters.size();
        }
        return argumentCount >= fixedParameters.size();
    }
}
