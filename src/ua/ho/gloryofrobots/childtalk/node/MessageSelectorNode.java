package ua.ho.gloryofrobots.childtalk.node;

public class MessageSelectorNode extends Node {

    private int mCountArguments = -1;

    public void setCountArguments(int count) {
        mCountArguments = count;
    }

    public int getCountArguments() {
        return mCountArguments;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s(%d)", mSelector, mCountArguments);
    }

    public String getSelector() {
        return mSelector;
    }

    public void setSelector(String mSelector) {
        this.mSelector = mSelector;
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }

    String mSelector;
}
