package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STMethod;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class BytecodeOperationSendToSuper extends BytecodeOperation {

    @Override public
    void perform(int countArguments){
        STStack stack = mRoutine.getStack();
        STSymbol selector = (STSymbol) stack.pop();
        
        STObject receiver = stack.getFromEnd(countArguments + 1);
        
        //Searching current method class and superclass
        Routine lastMethodRoutine = mRoutine.getLastMethodRoutine();
        STMethod method = (STMethod)lastMethodRoutine.getExecutable();
        STClass klass = method.getOwnerClass();
        STClass superClass = klass.getSuperClass();
         
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_INTERPRETER, "Send Message to super %s to %s",
                selector.toString(), receiver.toString());
        
        SchedulingSuite.callForSelector(mRoutine, superClass, selector);
    }

}
