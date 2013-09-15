package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperation;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationAssign;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationDuplicate;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationPopTop;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationPushArray;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationPushBlock;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationPushConstant;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationPushLiteral;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationPushObject;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationSelfReturn;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationSendMessage;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeOperationStackReturn;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeType;

public class InterpreterSuite {
    private static BytecodeOperation[] sOperations;

    static {
        int count = BytecodeType.values().length;
        sOperations = new BytecodeOperation[count];
        sOperations[BytecodeType.ASSIGN.ordinal()] = new BytecodeOperationAssign();
        sOperations[BytecodeType.DUPLICATE.ordinal()] = new BytecodeOperationDuplicate();
        sOperations[BytecodeType.POP_TOP.ordinal()] = new BytecodeOperationPopTop();

        sOperations[BytecodeType.PUSH_ARRAY.ordinal()] = new BytecodeOperationPushArray();
        sOperations[BytecodeType.PUSH_BLOCK.ordinal()] = new BytecodeOperationPushBlock();
        sOperations[BytecodeType.PUSH_LITERAL.ordinal()] = new BytecodeOperationPushLiteral();
        sOperations[BytecodeType.PUSH_CONSTANT.ordinal()] = new BytecodeOperationPushConstant();
        sOperations[BytecodeType.PUSH_OBJECT.ordinal()] = new BytecodeOperationPushObject();

        sOperations[BytecodeType.STACK_RETURN.ordinal()] = new BytecodeOperationStackReturn();
        sOperations[BytecodeType.SELF_RETURN.ordinal()] = new BytecodeOperationSelfReturn();
        sOperations[BytecodeType.SEND_MESSAGE.ordinal()] = new BytecodeOperationSendMessage();
    }

    public static BytecodeOperation get(int index) {
        return sOperations[index];
    }

    public static void performOperation(int index, int argument, Routine routine) {
        BytecodeOperation operation = get(index);
        System.out.printf("OP %s arg: %d\n", operation.getClass().getName(),
                argument);
        operation.setRoutine(routine);
        operation.perform(argument);
    }

    public static void add(BytecodeOperation operation) {

    }
}
