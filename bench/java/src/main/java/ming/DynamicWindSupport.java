package ming;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

final class DynamicWindSupport {
    @FunctionalInterface
    interface ThunkInvoker {
        Evaluator.Bounce invoke(Value thunk, Evaluator.Continuation cont) throws EvalError;
    }

    private final ThunkInvoker thunkInvoker;
    private WindFrame currentWind;

    DynamicWindSupport(ThunkInvoker thunkInvoker) {
        this.thunkInvoker = thunkInvoker;
    }

    WindFrame currentWind() {
        return currentWind;
    }

    void restore(WindFrame windFrame) {
        currentWind = windFrame;
    }

    Evaluator.Bounce apply(Value inThunk, Value bodyThunk, Value outThunk,
                           Evaluator.Continuation cont) throws EvalError {
        return invokeThunk(inThunk, ignoredValues -> enterBody(inThunk, bodyThunk, outThunk, cont));
    }

    Evaluator.Bounce transferTo(WindFrame targetWind, Evaluator.Bounce next)
            throws EvalError {
        return switchWind(targetWind, next);
    }

    private Evaluator.Bounce enterBody(Value inThunk, Value bodyThunk, Value outThunk,
                                       Evaluator.Continuation cont) throws EvalError {
        WindFrame frame = new WindFrame(currentWind, inThunk, outThunk);
        currentWind = frame;
        return invokeThunk(bodyThunk, bodyValues -> leaveBody(frame, bodyValues, cont));
    }

    private Evaluator.Bounce leaveBody(WindFrame frame, List<Value> bodyValues,
                                       Evaluator.Continuation cont) throws EvalError {
        currentWind = frame.parent();
        return invokeThunk(frame.outThunk(), ignoredValues -> deliver(cont, bodyValues));
    }

    private Evaluator.Bounce switchWind(WindFrame targetWind, Evaluator.Bounce next)
            throws EvalError {
        WindFrame commonWind = findCommonWind(currentWind, targetWind);
        List<WindFrame> exitFrames = collectWindFrames(currentWind, commonWind);
        List<WindFrame> entryFrames = collectWindFrames(targetWind, commonWind);
        return runWindExits(exitFrames, 0, entryFrames, entryFrames.size() - 1, next);
    }

    private Evaluator.Bounce runWindExits(List<WindFrame> exitFrames, int exitIndex,
                                          List<WindFrame> entryFrames, int entryIndex,
                                          Evaluator.Bounce next) throws EvalError {
        if (exitIndex >= exitFrames.size()) {
            return runWindEntries(entryFrames, entryIndex, next);
        }

        WindFrame frame = exitFrames.get(exitIndex);
        currentWind = frame.parent();
        return invokeThunk(frame.outThunk(), ignoredValues ->
                runWindExits(exitFrames, exitIndex + 1, entryFrames, entryIndex, next));
    }

    private Evaluator.Bounce runWindEntries(List<WindFrame> entryFrames, int entryIndex,
                                            Evaluator.Bounce next) throws EvalError {
        if (entryIndex < 0) {
            return next;
        }

        WindFrame frame = entryFrames.get(entryIndex);
        return invokeThunk(frame.inThunk(), ignoredValues -> {
            currentWind = frame;
            return runWindEntries(entryFrames, entryIndex - 1, next);
        });
    }

    private List<WindFrame> collectWindFrames(WindFrame top, WindFrame stopExclusive) {
        List<WindFrame> frames = new ArrayList<>();
        for (WindFrame frame = top; frame != stopExclusive; frame = frame.parent()) {
            frames.add(frame);
        }
        return frames;
    }

    private WindFrame findCommonWind(WindFrame left, WindFrame right) {
        IdentityHashMap<WindFrame, Boolean> leftAncestors = new IdentityHashMap<>();
        for (WindFrame frame = left; frame != null; frame = frame.parent()) {
            leftAncestors.put(frame, Boolean.TRUE);
        }
        for (WindFrame frame = right; frame != null; frame = frame.parent()) {
            if (leftAncestors.containsKey(frame)) {
                return frame;
            }
        }
        return null;
    }

    private Evaluator.Bounce deliver(Evaluator.Continuation cont, List<Value> values) {
        return () -> cont.resume(values);
    }

    private Evaluator.Bounce invokeThunk(Value thunk, Evaluator.Continuation cont)
            throws EvalError {
        return thunkInvoker.invoke(thunk, cont);
    }
}
