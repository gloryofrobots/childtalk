package ua.ho.gloryofrobots.yellowtalk.bytecode;

public enum BytecodeType {
    PUSH_OBJECT,
    PUSH_LITERAL,
    PUSH_CONSTANT,
    PUSH_ARRAY,
    PUSH_BLOCK,
    PUSH_SUPER,
    
    ASSIGN,
    
    
    MARK_ARGUMENTS,
    
    SEND_MESSAGE,
    SEND_MESSAGE_TO_SUPER,
    
    POP_TOP,
    
    SELF_RETURN,
    STACK_RETURN,
    BLOCK_RETURN,
    DUPLICATE,
    DO_EXTENDED;
    
    public enum Constant {
        NIL,TRUE,FALSE,SELF,SUPER;
    }
}
