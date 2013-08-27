package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.node.Node.StringWriter;

public class ClassNode extends Node implements NodeWithMetaData {

    public class VariableNames extends ArrayList<String> {
        private static final long serialVersionUID = -3930721623064440013L;
    }

    public List<MethodNode> mMethods;
    protected String m_superclassName = new String();
    protected String m_className = new String();
    protected String m_comment = new String();
    protected String m_category = new String();
    protected VariableNames m_instanceVariableNames;
    protected VariableNames m_classVariableNames;
    protected VariableNames m_poolDictionaries;

    public ClassNode() {
        super();
        mMethods = new LinkedList<MethodNode>();

        m_instanceVariableNames = new VariableNames();
        m_classVariableNames = new VariableNames();
        m_poolDictionaries = new VariableNames();
    }

    public void seClassName(String name) {
        m_superclassName = name;
    }

    public void setSuperclassName(String name) {
        m_superclassName = name;
    }

    public void setMetaData(String label, String value)
            throws NodeWithMetaData.UnknownMetaDataException {
        if (label.equals("comment:")) {
            setComment(value);
        } else if (label.equals("category:")) {
            setCategory(value);
        } else if (label.equals("instanceVariableNames:")) {
            parseVariableNames(m_instanceVariableNames, value);
        } else if (label.equals("classVariableNames:")) {
            parseVariableNames(m_classVariableNames, value);
        } else if (label.equals("poolDictionaries:")) {
            parseVariableNames(m_poolDictionaries, value);
        } else {
            throw new NodeWithMetaData.UnknownMetaDataException(String.format(
                    "Unknown metadata %s", label));
        }
    }

    public void parseVariableNames(VariableNames vars, String data) {
        String[] dataArray = data.split(" ");
        for (String varName : dataArray) {
            vars.add(varName);
        }
    }

    public void setComment(String comment) {
        m_comment = comment;
    }

    public void setCategory(String category) {
        m_category = category;
    }

    public void addMethod(MethodNode node) {
        mMethods.add(node);
    }

    // /PRINTING
    @Override
    protected void writeRepresentation(StringWriter writer) {
        writer.writeln("%s subclass: %s [", m_superclassName, m_className);
        writer.increaseLevel();

        writer.writeln("<category: '%s'>", m_category);
        writer.writeln("<comment: '%s'>", m_comment);
        writer.writeln("<instanceVariableNames: '%s'>",
                m_instanceVariableNames.toString());
        writer.writeln("<classVariableNames: '%s'>",
                m_classVariableNames.toString());
        writer.writeln("<poolDictionaries: '%s'>", m_poolDictionaries.toString());

        for (MethodNode method : mMethods) {
            writer.writeln("");
            method.writeRepresentation(writer);
        }
        writer.decreaseLevel();
        writer.writeln("-----------------");
    }

    /*
     * public void setInstanceVariable(String varName) {
     * 
     * }
     * 
     * public void setClassVariable(String varName) {
     * 
     * }
     * 
     * void setPoolDictionary(String poolDictionary) {
     * 
     * }
     */
}
