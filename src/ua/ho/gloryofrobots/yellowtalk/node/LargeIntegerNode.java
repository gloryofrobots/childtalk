package ua.ho.gloryofrobots.yellowtalk.node;

import java.math.BigInteger;

import ua.ho.gloryofrobots.yellowtalk.stobject.STLargeInteger;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
public class LargeIntegerNode extends Node implements NodeFactory{
    private BigInteger mData;

    public LargeIntegerNode(BigInteger data) {
        mData = data;
    }
    
    public BigInteger getData() {
        return mData;
    }
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s", mData.toString());
    }
    
    @Override
    public STObject createObject() {
        return  STLargeInteger.create(mData);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
