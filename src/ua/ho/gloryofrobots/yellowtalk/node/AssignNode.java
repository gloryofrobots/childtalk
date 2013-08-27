package ua.ho.gloryofrobots.yellowtalk.node;

public class AssignNode extends Node {
    
    public String getAssignName() {
        return mAssignName;
    }
    public void setAssignName(String mAssignName) {
        this.mAssignName = mAssignName;
    }
    public Node getValue() {
        return mValue;
    }
    public void setValue(StatementNode mValue) {
        this.mValue = mValue;
    }
    
    private String mAssignName;
    private StatementNode mValue;
    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("%s (:= %s)",mValue.toString(), mAssignName);
    }
   
}
