package ua.ho.gloryofrobots.yellowtalk.bytecode;

public enum BytecodeType {
    PUSH_OBJECT,
    PUSH_LITERAL,
    PUSH_CONSTANT,
    PUSH_ARRAY,
    PUSH_BLOCK,
    
    
    ASSIGN,
    
    
    MARK_ARGUMENTS,
    SEND_MESSAGE,
    SEND_TO_SUPER,
    SEND_TO_SELF,
    
    POP_TOP,
    
    SELF_RETURN,
    STACK_RETURN,
    DUPLICATE,
    DO_EXTENDED;
    
    public enum Constant {
        NIL,TRUE,FALSE,SELF,SUPER;
    }
}
