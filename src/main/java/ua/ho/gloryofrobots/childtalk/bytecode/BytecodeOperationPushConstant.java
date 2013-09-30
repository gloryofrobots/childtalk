package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public class BytecodeOperationPushConstant extends BytecodeOperation {

    @Override
    public
    void perform(int argument) {
        BytecodeType.Constant constant = BytecodeType.Constant.values()[argument];
        STStack stack = mRoutine.getStack();
        switch(constant) {
        case FALSE:
            stack.push(ImageSuite.image().objects().FALSE);
            break;
        case NIL:
            stack.push(ImageSuite.image().objects().NIL);
            break;
        case TRUE:
            stack.push(ImageSuite.image().objects().TRUE);
            break;
        case SELF: {
            STObject receiver = mRoutine.getContext().getReceiver();
            
            stack.push(receiver);
            break;
        }
        default:
            runtimeError("Unsupported constant %s", constant.toString());
        }
    }

}
