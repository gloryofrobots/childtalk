package ua.ho.gloryofrobots.yellowtalk.node;

public class CharacterNode extends Node {
    private char mData;

    public CharacterNode(char data) {
        mData = data;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%c", mData);
    }
}
