package ua.ho.gloryofrobots.yellowtalk.node;

public class MessageSelectorNode extends Node {

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s", mSelector);
    }

    public String getSelector() {
        return mSelector;
    }

    public void setSelector(String mSelector) {
        this.mSelector = mSelector;
    }

    String mSelector;
}
