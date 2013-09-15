package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushLiteral extends BytecodeOperation {

    @Override
    public 
    void perform(int argument) {
        STExecutableObject executable = mRoutine.getExecutable();
        STStack stack = mRoutine.getStack();
        STObject literal = executable.getLiteral(argument);
        if(literal == null) {
            runtimeError("Unknown literal");
        }
        
        stack.push(literal);
    }

}
