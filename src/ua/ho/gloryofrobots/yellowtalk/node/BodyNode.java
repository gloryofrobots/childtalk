package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

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
        for(StatementNode node : mStatements) {
            //writer.writeln("++++++++++++++++++++++");
            node.writeRepresentation(writer);
            
        }
        //writer.writeln("");
    }
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
