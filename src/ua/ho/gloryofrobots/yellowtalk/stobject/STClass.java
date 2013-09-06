package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.DuplicateVariableException;

public class STClass extends STObjectWithScope {
   
    private static final long serialVersionUID = 1L;
    
    STSymbol mComment;
    STSymbol mCategory;
    STSymbol mName;

    List<STSymbol> mInstanceVariables;

    public STClass() {
        mInstanceVariables = new ArrayList<STSymbol>();
    }
    
    public STMethod findMethod(STObject selector) {
        return mScope.getAndCast(selector);
    }
    
    public void setComment(STSymbol comment) {
        mComment = comment;
    }

    public void setCategory(STSymbol category) {
        mCategory = category;
    }

    public void setName(STSymbol name) {
        mName = name;
    }
    
    public STSymbol getName() {
        return mName ;
    }

    public void addInstanceVariable(STSymbol varName)
            throws DuplicateVariableException {
        if (mInstanceVariables.contains(varName)) {
            throw new DuplicateVariableException(varName.toString());
        }

        mInstanceVariables.add(varName);
    }

    public void addClassVariable(STSymbol varName)
            throws DuplicateVariableException {
        getScope().putUnique(varName, null);
    }

}
