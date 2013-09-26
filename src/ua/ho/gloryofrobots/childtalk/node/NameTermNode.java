package ua.ho.gloryofrobots.childtalk.node;

import ua.ho.gloryofrobots.childtalk.node.Node.StringWriter;

public class NameTermNode extends Node {
    public String getName() {
        return mName;
    }

    public void setName(String mName) {
        this.mName = mName;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s", mName);
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }

    private String mName;
}
