package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationBlockReturn extends BytecodeOperation {

    @Override
    public void perform(int argument) {
        STStack stack = mRoutine.getStack();
        STObject result = stack.pop();
        
        mRoutine.explicitCompleteWithResult(result);
    }

}
