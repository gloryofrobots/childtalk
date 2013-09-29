package ua.ho.gloryofrobots.childtalk.node;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.childtalk.compilation.Token;

public class BodyNode extends Node {

    public BodyNode() {
        mStatements = new ArrayList<StatementNode>();
        mLiterals = new ArrayList<String>();
    }

    public void addLiteral(String literal) {
        mLiterals.add(literal);
    }

    public void addStatement(StatementNode statement) {
        mStatements.add(statement);
    }

    protected List<String> mLiterals;
    protected List<StatementNode> mStatements;

    public List<StatementNode> getStatements() {
        return mStatements;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        int size = mStatements.size();
        int i = 1;
        for (StatementNode node : mStatements) {
            // writer.writeln("++++++++++++++++++++++");
            node.writeRepresentation(writer);
            if(i != size)
                writer.writeln("");
            i++;
        }
        // writer.writeln("");
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
