package ua.ho.gloryofrobots.childtalk.node;

public class MessageNode extends Node {
    
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
    
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
}
