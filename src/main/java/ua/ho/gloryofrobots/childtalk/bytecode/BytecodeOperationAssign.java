package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STScope;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public class BytecodeOperationAssign extends BytecodeOperation {

    @Override
    public void perform(int argument) {
        STExecutableObject executable = mRoutine.getExecutable();
        STObject varName = executable.getLiteral(argument);
        
        
        STStack stack = mRoutine.getStack();
        STObject value = stack.peek();
        
        STContext context = mRoutine.getContext();
        context.assign(varName, value);
    }

}
