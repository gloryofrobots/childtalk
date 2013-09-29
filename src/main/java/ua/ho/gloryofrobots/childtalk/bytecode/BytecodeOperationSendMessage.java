package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class BytecodeOperationSendMessage extends BytecodeOperation {
    @Override
    public
    void perform(int countArguments){
        STStack stack = mRoutine.getStack();
        STSymbol selector = (STSymbol) stack.pop();

        STObject receiver = stack.getFromEnd(countArguments + 1);
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_INTERPRETER, "Send Message %s to %s",
                selector.toString(), receiver.toString());
        
        SchedulingSuite.callForSelector(mRoutine, receiver.getSTClass(), selector);
    }
}
