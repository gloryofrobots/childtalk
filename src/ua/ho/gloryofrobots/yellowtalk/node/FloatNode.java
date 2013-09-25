package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.node.Node.StringWriter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STCharacter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STFloating;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public class FloatNode extends Node implements NodeFactory{
    private double mData;

    public FloatNode(double data) {
        mData = data;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%f", mData);
    }

    public double getData() {
        return mData;
    }
    
    @Override
    public STObject createObject() {
        return STFloating.create(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
