package ua.ho.gloryofrobots.yellowtalk.node;

public class StringNode extends Node {
    private String mData;

    public StringNode(String data) {
        mData = data;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write(mData);
    }
}
