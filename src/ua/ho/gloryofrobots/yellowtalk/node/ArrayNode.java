package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.List;

public class ArrayNode extends Node {
    
    public void addElement(Node element) {
        mElements.add(element);
    }
    
    List<Node> mElements;

    @Override
    void writeRepresentation(StringWriter writer) {
        // TODO Auto-generated method stub
        writer.write("{%s}", mElements);
    }
}
