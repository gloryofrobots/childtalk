package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.stobject.STFloating;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;

public class StringNode extends Node implements NodeFactory{
    private String mData;

    public StringNode(String data) {
        mData = data;
    }

    public String getData() {
        return mData;
    }

    @Override
    public STObject createObject() {
        // TODO Auto-generated method stub
        return  STString.create(mData);
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
