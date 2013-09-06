package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationAssign extends BytecodeOperation {

    @Override
    void perform(int argument) {
        STExecutableObject executable = mRoutine.getExecutable();
        STObject varName = executable.getLiteral(argument);
        
        
        STStack stack = mRoutine.getStack();
        STObject value = stack.peek();
        
        STContext context = mRoutine.getContext();
        context.assign(varName, value);
    }

}
