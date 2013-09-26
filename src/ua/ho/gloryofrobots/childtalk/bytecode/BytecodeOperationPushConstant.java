package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.Universe;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
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
            stack.push(Universe.objects().FALSE);
            break;
        case NIL:
            stack.push(Universe.objects().NIL);
            break;
        case TRUE:
            stack.push(Universe.objects().TRUE);
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
