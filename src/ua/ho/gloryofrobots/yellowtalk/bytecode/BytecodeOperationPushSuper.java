package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushSuper extends BytecodeOperation {

    @Override
    public void perform(int argument) {
        STContext context = mRoutine.getContext();
        STStack stack = mRoutine.getStack();
        //We always lookup for super because we can be in block now.
        STObject obj = context.lookup(Universe.symbols().SUPER);
        if(obj == null) {
            runtimeError("super not exist in scope");
        }
        
        stack.push(obj);
    }

}
