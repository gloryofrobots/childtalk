package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

public class StatementNode extends Node {

    public StatementNode() {
        mNodes = new ArrayList<Node>();
    }
    
    public void addNode(Node node) {
        mNodes.add(node);
    }
    
    public List<Node> getNodes() {
        return mNodes;
    }
    
    public void onCascade() {
        Node firstNode = mNodes.get(0);
        mNodes.add(firstNode);
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        int last = mNodes.size();
        StringWriter nodeWriter = new StringWriter();
        nodeWriter.setPadding(2);
        for (int i = 0; i < last; i++) {
            Node node = mNodes.get(i);
            nodeWriter.write(node.toString());
        }
        writer.write(nodeWriter);
    }

    private List<Node> mNodes;

    public int getSize() {
        return mNodes.size();
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
