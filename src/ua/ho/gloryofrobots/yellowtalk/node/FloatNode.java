package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.node.Node.StringWriter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STCharacter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STFloat;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public class FloatNode extends Node implements NodeFactory{
    private float mData;

    public FloatNode(float data) {
        mData = data;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%f", mData);
    }

    public float getData() {
        return mData;
    }
    
    @Override
    public STObject createObject() {
        // TODO Auto-generated method stub
        return new STFloat(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
