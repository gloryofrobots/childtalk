package ua.ho.gloryofrobots.childtalk.node;

import java.util.LinkedList;
import java.util.List;


public abstract class MethodsContainerNode extends Node {
    public List<MethodNode> mMethods;
    MethodsContainerNode() {
        mMethods = new LinkedList<MethodNode>();
    }
    
    @Override
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
    
    public void addMethod(MethodNode node) {
        mMethods.add(node);
    }

    public List<MethodNode> getMethods() {
        return mMethods;
    }
    
}
