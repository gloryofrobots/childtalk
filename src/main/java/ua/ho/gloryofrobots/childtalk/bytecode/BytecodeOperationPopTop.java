package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public class BytecodeOperationPopTop extends BytecodeOperation{

    @Override
    public void perform(int argument) {
        // TODO Auto-generated method stub
        STStack stack = mRoutine.getStack();
        stack.pop();
    }

}
