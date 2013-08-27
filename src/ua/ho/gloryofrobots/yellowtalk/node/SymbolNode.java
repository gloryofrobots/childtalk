package ua.ho.gloryofrobots.yellowtalk.node;

public class SymbolNode extends Node {
    private String mData;

    public SymbolNode(String data) {
        mData = data;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write(mData);
    }
}
