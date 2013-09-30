package ua.ho.gloryofrobots.childtalk.node;

import ua.ho.gloryofrobots.childtalk.stobject.STFloating;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class FloatNode extends Node implements NodeFactory {
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
