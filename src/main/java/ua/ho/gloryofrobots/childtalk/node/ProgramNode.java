package ua.ho.gloryofrobots.childtalk.node;

import java.util.ArrayList;
import java.util.List;

public class ProgramNode extends Node {
    protected List<Node> mNodes;
    
    public ProgramNode() {
        mNodes = new ArrayList<Node>();
    }
    @Override
    void writeRepresentation(StringWriter writer) {
        // TODO Auto-generated method stub
        for(Node node : mNodes) {
            writer.write("%s\n", node.toString());
        }
    }
    
    public List<Node> getNodes() {
        return mNodes;
    }
    
    public void addNode(Node node) {
        mNodes.add(node);
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
   
    public int size() {
       return mNodes.size();
    }
    public Node getNode(int i) {
        return mNodes.get(i);
    }
}
