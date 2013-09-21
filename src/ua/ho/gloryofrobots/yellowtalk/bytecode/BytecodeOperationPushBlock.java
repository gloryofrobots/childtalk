package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushBlock extends BytecodeOperation {

    @Override
    public
    void perform(int argument) {
        STExecutableObject executable = mRoutine.getExecutable();
        STStack stack = mRoutine.getStack();
        STBlock block = (STBlock) executable.getLiteral(argument);
        STBlock newBlock = block.copySelf();
        newBlock.attachToRoutine(mRoutine);
        stack.push(newBlock);
    }

}
