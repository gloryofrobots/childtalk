package ua.ho.gloryofrobots.yellowtalk.node;

public class CascadeNode extends Node {
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write(";");
    }

    @Override
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
