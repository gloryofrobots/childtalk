package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

public class ProgramNode extends Node {
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

    public void addNode(Node node) {
        mNodes.add(node);
    }

    protected List<Node> mNodes;
}
