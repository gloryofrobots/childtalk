package ua.ho.gloryofrobots.yellowtalk.node;

public class MessageNode2 extends Node {
    
    StatementNode mStatement;
    public StatementNode getStatement() {
        return mStatement;
    }
    public void setStatement(StatementNode mStatement) {
        this.mStatement = mStatement;
    }
    public String getSelector() {
        return mSelector;
    }
    public void setSelector(String mSelector) {
        this.mSelector = mSelector;
    }
    String mSelector;
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s %s", mStatement, mSelector);
    }
}
