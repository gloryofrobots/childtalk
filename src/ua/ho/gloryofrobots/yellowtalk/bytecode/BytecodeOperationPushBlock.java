package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;

public class BytecodeOperationPushBlock extends BytecodeOperation {

    @Override
    void perform(int argument) {
        STExecutableObject executable = mRoutine.getExecutable();
        STContext context = mRoutine.getContext();
        STBlock block = (STBlock) executable.getLiteral(argument);
        block.attachToRoutine(mRoutine);
    }

}
