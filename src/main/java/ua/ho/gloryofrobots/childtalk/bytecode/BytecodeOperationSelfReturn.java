package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class BytecodeOperationSelfReturn extends BytecodeOperation {
    @Override
    public
    void perform(int argument) {
        STContext context  = mRoutine.getContext();
        STObject result = context.getReceiver();
        mRoutine.compliteWithResult(result);
    }

}
