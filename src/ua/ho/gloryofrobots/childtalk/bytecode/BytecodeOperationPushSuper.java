package ua.ho.gloryofrobots.childtalk.bytecode;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public class BytecodeOperationPushSuper extends BytecodeOperation {

    @Override
    public void perform(int argument) {
        STContext context = mRoutine.getContext();
        STStack stack = mRoutine.getStack();
        //We always lookup for super because we can be in block now.
        STObject obj = context.lookup(ImageSuite.image().symbols().SUPER);
        if(obj == null) {
            runtimeError("super not exist in scope");
        }
        
        stack.push(obj);
    }

}
