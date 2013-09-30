package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.stobject.STBlock;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

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
