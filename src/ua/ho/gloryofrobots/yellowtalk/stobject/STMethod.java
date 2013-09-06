package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.LinkedList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.DuplicateVariableException;

public class STMethod extends STExecutableObject {

    private static final long serialVersionUID = 1L;

    private List<STSymbol> mTemporaries;

    STSymbol mComment;
    STSymbol mCategory;
    STSymbol mPrimitive;

    public STMethod() {
        mTemporaries = new LinkedList<STSymbol>();
    }

    public void setComment(STSymbol comment) {
        mComment = comment;
    }

    public void setCategory(STSymbol category) {
        mCategory = category;
    }

    public void setPrimitive(STSymbol primitive) {
        mPrimitive = primitive;
    }

    public void addTemporary(STSymbol name) throws DuplicateVariableException {
        if (mTemporaries.contains(name)) {
            throw new DuplicateVariableException(name.toString());
        }

        mTemporaries.add(name);
    }

    @Override
    public void fillScope(STScope scope) {
        super.fillScope(scope);

        for (STSymbol varName : mTemporaries) {
            scope.put(varName, null);
        }
    }
}
