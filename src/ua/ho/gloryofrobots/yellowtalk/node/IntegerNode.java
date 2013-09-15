package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.stobject.STSmallInteger;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;


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
        // TODO Auto-generated method stub
        return  STSmallInteger.create(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
