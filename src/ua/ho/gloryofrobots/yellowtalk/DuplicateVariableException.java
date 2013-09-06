package ua.ho.gloryofrobots.yellowtalk;

public class DuplicateVariableException extends Exception{
    
    private static final long serialVersionUID = 1L;
    
    String mVarName;
    
    String getVariableName() {
        return mVarName;
    }
    
    public DuplicateVariableException(String name) {
        super();
        mVarName = name;
    }
}