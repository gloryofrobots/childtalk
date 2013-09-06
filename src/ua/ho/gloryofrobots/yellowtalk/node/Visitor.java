package ua.ho.gloryofrobots.yellowtalk.node;

public interface Visitor  {
    public void visit(LargeIntegerNode node);
    public void visit(IntegerNode node);
    public void visit(FloatNode node);
    public void visit(CharacterNode node);
    public void visit(SymbolNode node);
    public void visit(StringNode node);
    public void visit(LiteralArrayNode node);
    
    
    public void visit(BodyNode node);
    public void visit(BlockNode node);
    public void visit(AssignNode node);
    
    public void visit(ArrayNode node);
        
    public void visit(StatementNode node);
    public void visit(ReturnNode node);
    public void visit(Node node);
    public void visit(NameTermNode node);
    public void visit(MessageSelectorNode node);
    
    
}
