package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationStackReturn extends BytecodeOperation {

    @Override
    void perform(int argument) throws BytecodeRuntimeError {
        STStack stack = mRoutine.getStack();
        STObject result = stack.pop();
        mRoutine.compliteWithResult(result);
    }

}
