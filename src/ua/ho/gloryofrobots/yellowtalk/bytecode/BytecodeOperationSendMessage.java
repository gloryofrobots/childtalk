package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class BytecodeOperationSendMessage extends BytecodeOperation {
    @Override
    public
    void perform(int countArguments){
        STStack stack = mRoutine.getStack();
        STSymbol selector = (STSymbol) stack.pop();

        STObject receiver = stack.getFromEnd(countArguments + 1);
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_INTERPRETER, "Send Message %s to %s",
                selector.toString(), receiver.toString());
        SchedulingSuite.callForSelector(mRoutine, receiver, selector);
    }
}
