package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STArray;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushArray extends BytecodeOperation {

    @Override
    public
    void perform(int argument) {
        STStack stack = mRoutine.getStack();
        STArray array = STArray.create(argument);

        for (int i = 1; i <= argument; i++) {
            STObject obj = stack.pop();
            array.put(argument - i, obj);
        }

        stack.push(array);
    }

}
