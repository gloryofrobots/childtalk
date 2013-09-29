package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.STBlock;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public class BytecodeOperationBlockReturn extends BytecodeOperation {

    @Override
    public void perform(int argument) {
        STStack stack = mRoutine.getStack();
        STObject result = stack.pop();
        
        mRoutine.explicitCompleteWithResult(result);
    }

}
