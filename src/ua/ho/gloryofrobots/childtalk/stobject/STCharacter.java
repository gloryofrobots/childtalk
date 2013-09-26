package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.Universe;

public class STCharacter extends STObject {
 
    private static final long serialVersionUID = 1L;
    private char mValue;

    protected STCharacter(char data) {
        mValue = data;
    }
    
    public char toChar() {
        return mValue;
    }

    public static STCharacter create(char value) {
        STCharacter character = new STCharacter(value);
        character.setSTClass(Universe.classes().Character);
        return character;
    }
    
    public String toString() {
        String data = "$" + mValue;
        return data;
    }
}
