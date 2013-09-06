package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.stobject.STArray;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public class ArrayNode extends Node implements NodeFactory{

    public void addElement(Node element) {
        mElements.add(element);
    }

    public List<Node> getElements() {
        return mElements;
    }

    public int getSize() {
        return mElements.size();
    }

    @Override
    public STObject createObject() {
        STArray array = new STArray(getSize());

        for (Node element : mElements) {

            NodeFactory factory = (NodeFactory) element;
            STObject obj = factory.createObject();
            array.add(obj);

        }
        return array;
    }

    List<Node> mElements;

    @Override
    void writeRepresentation(StringWriter writer) {
        // TODO Auto-generated method stub
        writer.write("{%s}", mElements);
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
