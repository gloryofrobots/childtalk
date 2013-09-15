package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationStackReturn extends BytecodeOperation {

    @Override
    public
    void perform(int argument)  {
        STStack stack = mRoutine.getStack();
        STObject result = stack.pop();
        mRoutine.compliteWithResult(result);
    }

}
