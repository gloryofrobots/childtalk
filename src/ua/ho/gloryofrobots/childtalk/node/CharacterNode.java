package ua.ho.gloryofrobots.childtalk.node;

import ua.ho.gloryofrobots.childtalk.stobject.STCharacter;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class CharacterNode extends Node implements NodeFactory{
    private char mData;

    public CharacterNode(char data) {
        mData = data;
    }
    
    public char getData() {
        return mData;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%c", mData);
    }

    @Override
    public STObject createObject() {
        return  STCharacter.create(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
