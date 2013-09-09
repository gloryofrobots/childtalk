package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.LinkedList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.MethodRoutine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.PrimitiveRoutine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;

public class STMethod extends STExecutableObject {

    private static final long serialVersionUID = 1L;

    private STArray mTemporaries;

    STSymbol mComment;
    STSymbol mCategory;
    STSymbol mPrimitiveName = null;
    
    public STMethod() {
        mTemporaries = new STArray();
    }
    
    public void setComment(STSymbol comment) {
        mComment = comment;
    }
    
    public void setCategory(STSymbol category) {
        mCategory = category;
    }

    public void setPrimitiveName(STSymbol primitive) {
        mPrimitiveName = primitive;
    }
    
    public STSymbol getPrimitiveName() {
        return mPrimitiveName;
    }
    
    public boolean hasPrimitive() {
        if(mPrimitiveName == null){
            return false;
        }
        
        return true;
    }
    
    public void addTemporary(STSymbol name) throws DuplicateVariableException {
        if (mTemporaries.has(name)) {
            throw new DuplicateVariableException(name.toString());
        }

        mTemporaries.add(name);
    }

    @Override
    public void fillScope(STScope scope) {
        super.fillScope(scope);
        
        int size = mTemporaries.size();
        for(int i = 0; i < size; i++) {
            STSymbol varName  = mTemporaries.getAndCast(i);
            scope.put(varName, null);
        }
    }

    @Override
    public Routine createRoutine() {
        Routine routine;
        
        if (hasPrimitive() == false) {
            routine = new MethodRoutine(this);
        } else {
            routine = new PrimitiveRoutine(this);
        }
        
        return routine;
    }
}
