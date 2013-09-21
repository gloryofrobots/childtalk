package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public class BytecodeOperationSelfReturn extends BytecodeOperation {
    @Override
    public
    void perform(int argument) {
        STContext context  = mRoutine.getContext();
        STObject result = context.getReceiver();
        mRoutine.compliteWithResult(result);
    }

}
