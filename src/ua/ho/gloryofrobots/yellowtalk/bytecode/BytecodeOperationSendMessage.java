package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class BytecodeOperationSendMessage extends BytecodeOperation {

    @Override
    void perform(int countArguments) throws BytecodeRuntimeError {
        STStack stack = mRoutine.getStack();
        STObject selector = stack.pop();
        
        STObject receiver = stack.getFromEnd(countArguments);
        STClass superClass = receiver.getSuperClass();
        STMethod method = superClass.findMethod(selector);
        
        //FIXME
        if(method == null) {
            runtimeError("Unknown method %s in class %s", ((STSymbol)selector).toString(),
                    ((STSymbol)superClass.getName()).toString());
        }
        
        Routine routine = new Routine(method);
        routine.callFrom(mRoutine);
    }
}
