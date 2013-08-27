package ua.ho.gloryofrobots.yellowtalk.node;

import java.math.BigInteger;
public class LargeIntegerNode extends Node {
    private BigInteger mData;

    public LargeIntegerNode(BigInteger data) {
        mData = data;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s", mData.toString());
    }
}
