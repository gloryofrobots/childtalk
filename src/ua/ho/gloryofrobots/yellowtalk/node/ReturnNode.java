package ua.ho.gloryofrobots.yellowtalk.node;


public class ReturnNode extends Node {
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("^");
    }
}
