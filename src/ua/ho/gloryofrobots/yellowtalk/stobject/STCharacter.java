package ua.ho.gloryofrobots.yellowtalk.stobject;

public class STCharacter extends STObject {
 
    private static final long serialVersionUID = 1L;
    private char mValue;

    public STCharacter(char data) {
        mValue = data;
    }
    
    public char getValue() {
        return mValue;
    }
}
