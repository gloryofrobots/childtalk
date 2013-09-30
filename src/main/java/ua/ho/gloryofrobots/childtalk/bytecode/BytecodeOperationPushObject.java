package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public class BytecodeOperationPushObject extends BytecodeOperation {

    @Override
    public
    void perform(int argument)  {
        STContext context = mRoutine.getContext();
        STObject name = mRoutine.getExecutable().getLiteral(argument);
      
        STObject obj = context.lookup(name);
        if(obj == null) {
            obj = context.lookup(name);
            runtimeError("Uknown name %s", name.toString());
        }
        
        STStack stack = mRoutine.getStack();
        stack.push(obj);
    }

}
