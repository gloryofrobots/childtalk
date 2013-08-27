package ua.ho.gloryofrobots.yellowtalk.node;


public class IntegerNode extends Node {
    private int mData;

    public IntegerNode(int data) {
        mData = data;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%d", mData);
    }
}
