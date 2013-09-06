package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public class BytecodeOperationPushConstant extends BytecodeOperation {

    @Override
    void perform(int argument) throws BytecodeRuntimeError {
        // TODO Auto-generated method stub
        
        BytecodeType.Constant constant = BytecodeType.Constant.values()[argument];
        STStack stack = mRoutine.getStack();
        switch(constant) {
        case FALSE:
            stack.push(Universe.objects.FALSE);
            break;
        case NIL:
            stack.push(Universe.objects.NIL);
            break;
        case TRUE:
            stack.push(Universe.objects.TRUE);
            break;
        default:
            runtimeError("Unsopported constant %s", constant.toString());
        }
    }

}
