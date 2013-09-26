package ua.ho.gloryofrobots.childtalk.node;

import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STSmallInteger;


public class IntegerNode extends Node implements NodeFactory{
    private int mData;

    public IntegerNode(int data) {
        mData = data;
    }
    
    public int getData() {
        return mData;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%d", mData);
    }
    
    @Override
    public STObject createObject() {
        return  STSmallInteger.create(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
