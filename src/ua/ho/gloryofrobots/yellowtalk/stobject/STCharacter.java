package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;

public class STCharacter extends STObject {
 
    private static final long serialVersionUID = 1L;
    private char mValue;

    private STCharacter(char data) {
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
}