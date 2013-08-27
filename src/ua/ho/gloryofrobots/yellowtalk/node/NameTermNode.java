package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.node.Node.StringWriter;

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

    private String mName;
}
