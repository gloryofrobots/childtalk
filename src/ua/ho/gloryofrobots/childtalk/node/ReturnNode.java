package ua.ho.gloryofrobots.childtalk.node;

public class ReturnNode extends Node {
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("^");
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
