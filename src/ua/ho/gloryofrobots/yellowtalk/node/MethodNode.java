package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.node.Node.StringWriter;

public class MethodNode extends Node implements NodeWithMetaData {

    public MethodNode() {
        mBody = new BodyNode();
        mArguments = new ArrayList<String>();
        mTemporaries = new ArrayList<String>();
    }

    @Override
    public void setMetaData(String label, String value)
            throws UnknownMetaDataException {
        if (label.equals("comment:")) {
            setComment(value);
        } else if (label.equals("category:")) {
            setCategory(value);
        }

    }

    public String getSelector() {
        return mSelector;
    }

    public void setSelector(String mSelector) {
        this.mSelector = mSelector;
    }

    public List<String> getArguments() {
        return mArguments;
    }

    public void addArgument(String argument) {
        mArguments.add(argument);
    }

    public void addTemporary(String temporary) {
        mTemporaries.add(temporary);
    }

    public List<String> getTemporaries() {
        return mTemporaries;
    }

    public BodyNode getBody() {
        return mBody;
    }

    public String getComment() {
        return mComment;
    }

    public void setComment(String mComment) {
        this.mComment = mComment;
    }

    public String getCategory() {
        return mCategory;
    }

    public void setCategory(String mCategory) {
        this.mCategory = mCategory;
    }

    public String getPrimitiveName() {
        return mPrimitiveName;
    }

    public void setPrimitiveName(String mPrimitiveName) {
        this.mPrimitiveName = mPrimitiveName;
    }

    // /PRINTING
    @Override
    protected void writeRepresentation(StringWriter writer) {
        writer.writeln("selector:%s arguments:%s [", mSelector,
                mArguments.toString());
        writer.increaseLevel();
        writer.writeln("|%s|", mTemporaries.toString());

        if (mCategory.length() > 0) {
            writer.writeln("<category: '%s'>", mCategory);
        }

        if (mComment.length() > 0) {
            writer.writeln("<comment: '%s'>", mComment);
        }

        if (mPrimitiveName.length() > 0) {
            writer.writeln("<primitive: '%s'>", mPrimitiveName);
        }

        mBody.writeRepresentation(writer);
        writer.decreaseLevel();
        writer.writeln("]");
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
    
    BodyNode mBody;
    protected String mComment = new String();
    protected String mCategory = new String();
    protected String mSelector = new String();
    protected String mPrimitiveName = new String();
    protected List<String> mArguments;
    protected List<String> mTemporaries;

}
