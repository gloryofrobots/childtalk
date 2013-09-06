package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;

public class BytecodeInterpreter {
    private static BytecodeOperation[] mOperations;
    
    public static void init() {
        int count = BytecodeType.values().length;
        mOperations = new  BytecodeOperation[count];
        mOperations[BytecodeType.ASSIGN.ordinal()] = new BytecodeOperationAssign();
        mOperations[BytecodeType.DUPLICATE.ordinal()] = new BytecodeOperationDuplicate();
        mOperations[BytecodeType.POP_TOP.ordinal()] = new BytecodeOperationPopTop();
        
        mOperations[BytecodeType.PUSH_ARRAY.ordinal()] = new BytecodeOperationPushArray();
        mOperations[BytecodeType.PUSH_BLOCK.ordinal()] = new BytecodeOperationPushBlock();
        mOperations[BytecodeType.PUSH_LITERAL.ordinal()] = new BytecodeOperationPushLiteral();
        mOperations[BytecodeType.PUSH_CONSTANT.ordinal()] = new BytecodeOperationPushConstant();
        mOperations[BytecodeType.PUSH_OBJECT.ordinal()] = new BytecodeOperationPushObject();
        
        mOperations[BytecodeType.SELF_RETURN.ordinal()] = new BytecodeOperationSelfReturn();
        mOperations[BytecodeType.SEND_MESSAGE.ordinal()] = new BytecodeOperationSendMessage();
    }
    
    public static BytecodeOperation get(int index) {
        return null;
    }
    
    public static void performOperation(int index, int argument, Routine routine) {
    }
    
    public static void add(BytecodeOperation operation) {
        
    }
}
