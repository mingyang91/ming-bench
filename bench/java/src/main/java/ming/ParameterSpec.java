package ming;

import java.util.List;

record ParameterSpec(List<String> fixedParameters, String restParameter) {
    ParameterSpec {
        fixedParameters = List.copyOf(fixedParameters);
    }
}
