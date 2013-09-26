package ua.ho.gloryofrobots.childtalk.node;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.childtalk.stobject.STArray;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class ArrayNode extends Node implements NodeFactory{
    List<Node> mElements;

    public ArrayNode() {
        super();
        mElements = new ArrayList<Node>();
    }
    
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
    public STObject createObject() throws NodeFactoryException {
        int size = getSize();
        STArray array =  STArray.create(size);
        
        for (Node element : mElements) {
            NodeFactory factory = (NodeFactory) element;
            STObject obj = factory.createObject();
            array.add(obj);

        }
        
        return array;
    }

    
    @Override
    void writeRepresentation(StringWriter writer) {
        // TODO Auto-generated method stub
        writer.write("{");
        for(Node node : mElements) {
            writer.write(node.toString());
        }
        writer.write("}");
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
