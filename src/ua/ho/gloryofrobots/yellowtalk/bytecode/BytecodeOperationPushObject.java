package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushObject extends BytecodeOperation {

    @Override
    void perform(int argument) throws BytecodeRuntimeError {
        STContext context = mRoutine.getContext();
        STScope scope = context.getScope();
        STObject name = mRoutine.getExecutable().getLiteral(argument);
        STObject obj = scope.at(name);
        if(obj == null) {
            runtimeError("Uknown name %s", name.toString());
        }
        
        STStack stack = mRoutine.getStack();
        stack.push(obj);
    }

}
