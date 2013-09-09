package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class BytecodeOperationSendMessage extends BytecodeOperation {

    @Override
    void perform(int countArguments) throws BytecodeRuntimeError {
        STStack stack = mRoutine.getStack();
        STSymbol selector = (STSymbol) stack.pop();

        STObject receiver = stack.getFromEnd(countArguments);
        Routine.call(mRoutine, receiver, selector);
    }

   
}
