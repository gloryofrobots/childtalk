package ua.ho.gloryofrobots.childtalk.node;

import java.util.List;

import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.stobject.STMethod;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class MethodNode extends ExecutableNode implements NodeWithMetaData,
        NodeFactory {

    protected String mComment = new String();
    protected String mClassName;
    protected String mCategory = new String();
    protected String mSelector = new String();
    protected String mPrimitiveName = new String();
    private boolean mIsStatic = false;

    public void setStatic(boolean value) {
        mIsStatic = value;
    }

    public boolean isStatic() {
        return mIsStatic;
    }

    @Override
    public void setMetaData(String label, String value)
            throws UnknownMetaDataException {
        if (label.equals("comment:")) {
            setComment(value);
        } else if (label.equals("category:")) {
            setCategory(value);
        } else if (label.equals("primitive:")) {
            setPrimitiveName(value);
        }
    }

    public String getClassName() {
        return mClassName;
    }

    public void setClassName(String mClassName) {
        this.mClassName = mClassName;
    }

    public String getSelector() {
        return mSelector;
    }

    public void setSelector(String mSelector) {
        this.mSelector = mSelector;
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
        if (mPrimitiveName.length() == 0) {
            int bdsm = 1;
            int x = bdsm;
        }
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

    // TODO REMOVE UNNECESSARY METHODS
    @Override
    public STObject createObject() throws NodeFactoryException {
        STMethod method = STMethod.create();
        method.setCompileInfo(mCompileInfo);

        String selector = getSelector();
        if (selector != null) {
            method.setSelector(STSymbol.create(selector));
        }

        String className = getClassName();
        if (className != null) {
            method.setClassName(STSymbol.create(className));
        }

        String comment = getComment();
        if (comment != null) {
            method.setComment(STSymbol.create(comment));
        }

        String category = getCategory();
        if (category != null) {
            method.setCategory(STSymbol.create(category));
        }

        if (mPrimitiveName.length() > 0) {
            method.setPrimitiveName(STSymbol.create(mPrimitiveName));
        }

        List<String> temporaries = getTemporaries();
        for (String varName : temporaries) {
            try {
                method.addTemporary(STSymbol.create(varName));
            } catch (DuplicateVariableException e) {
                throw new NodeFactoryException(
                        "Duplicate method temporary variable " + varName);

            }
        }

        List<String> arguments = getArguments();

        for (String varName : arguments) {
            try {
                method.addArgument(STSymbol.create(varName));
            } catch (DuplicateVariableException e) {
                throw new NodeFactoryException("Duplicate method argument "
                        + varName);
            }
        }

        return method;
    }
}
