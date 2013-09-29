package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

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
        character.setClassProvider(new BindingClassProvider(character) {
            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().Character;
            }
        });
        return character;
    }
    
    public String toString() {
        String data = "$" + mValue;
        return data;
    }
}
