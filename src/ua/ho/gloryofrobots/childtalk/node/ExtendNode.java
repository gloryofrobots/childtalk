package ua.ho.gloryofrobots.childtalk.node;


public class ExtendNode extends MethodsContainerNode {
    protected String m_className = new String();

    public String getClassName() {
        return m_className;
    }

    public void setClassName(String name) {
        m_className = name;
    }

    @Override
    protected void writeRepresentation(StringWriter writer) {
        writer.writeln("%s  extend [", m_className);
        writer.increaseLevel();

        for (MethodNode method : mMethods) {
            writer.writeln("");
            method.writeRepresentation(writer);
        }
        writer.decreaseLevel();
        writer.writeln("]");
    }

    @Override
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }

}
