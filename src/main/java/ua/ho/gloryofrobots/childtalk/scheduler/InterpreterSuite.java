package ua.ho.gloryofrobots.childtalk.scheduler;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperation;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationAssign;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationBlockReturn;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationDuplicate;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPopTop;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPushArray;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPushBlock;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPushConstant;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPushLiteral;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPushObject;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationPushSuper;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationSelfReturn;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationSendMessage;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationSendToSuper;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeOperationStackReturn;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeType;

public class InterpreterSuite {
    private static BytecodeOperation[] sOperations;

    static {
        int count = BytecodeType.values().length;
        sOperations = new BytecodeOperation[count];
        sOperations[BytecodeType.ASSIGN.ordinal()] = new BytecodeOperationAssign();
        //sOperations[BytecodeType.DUPLICATE.ordinal()] = new BytecodeOperationDuplicate();
        sOperations[BytecodeType.POP_TOP.ordinal()] = new BytecodeOperationPopTop();
        
        sOperations[BytecodeType.PUSH_ARRAY.ordinal()] = new BytecodeOperationPushArray();
        sOperations[BytecodeType.PUSH_BLOCK.ordinal()] = new BytecodeOperationPushBlock();
        sOperations[BytecodeType.PUSH_LITERAL.ordinal()] = new BytecodeOperationPushLiteral();
        sOperations[BytecodeType.PUSH_CONSTANT.ordinal()] = new BytecodeOperationPushConstant();
        sOperations[BytecodeType.PUSH_OBJECT.ordinal()] = new BytecodeOperationPushObject();
        sOperations[BytecodeType.PUSH_SUPER.ordinal()] = new BytecodeOperationPushSuper();
        
        sOperations[BytecodeType.STACK_RETURN.ordinal()] = new BytecodeOperationStackReturn();
        sOperations[BytecodeType.SELF_RETURN.ordinal()] = new BytecodeOperationSelfReturn();
        sOperations[BytecodeType.BLOCK_RETURN.ordinal()] = new BytecodeOperationBlockReturn();
        
        sOperations[BytecodeType.SEND_MESSAGE.ordinal()] = new BytecodeOperationSendMessage();
        sOperations[BytecodeType.SEND_MESSAGE_TO_SUPER.ordinal()] = new BytecodeOperationSendToSuper();
    }

    public static BytecodeOperation get(int index) {
        return sOperations[index];
    }

    public static void performOperation(int index, int argument, Routine routine) {
        BytecodeOperation operation = get(index);

        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_BYTECODE,
                "OP %s arg: %d", operation.getClass().getSimpleName(), argument);
        /*DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_INTERPRETER,
                routine.getStack().toString() + "\n");*/
        operation.setRoutine(routine);
        operation.perform(argument);
    }
}
