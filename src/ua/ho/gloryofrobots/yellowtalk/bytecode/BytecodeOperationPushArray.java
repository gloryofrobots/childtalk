package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STArray;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushArray extends BytecodeOperation {

    @Override
    void perform(int argument) {
        STStack stack = mRoutine.getStack();
        STArray array = new STArray(argument);

        for (int i = 1; i <= argument; i++) {
            STObject obj = stack.pop();
            array.set(argument - i, obj);
        }

        stack.push(array);
    }

}
