package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPopTop extends BytecodeOperation{

    @Override
    void perform(int argument) {
        // TODO Auto-generated method stub
        STStack stack = mRoutine.getStack();
        stack.pop();
    }

}
