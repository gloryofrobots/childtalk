package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.node.Node.StringWriter;

public class FloatNode extends Node {
    private float mData;

    public FloatNode(float data) {
        mData = data;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%f", mData);
    }
}
