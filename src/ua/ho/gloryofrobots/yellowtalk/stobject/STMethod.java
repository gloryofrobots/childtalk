package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.scheduler.MethodRoutine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.PrimitiveRoutine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STMethod extends STExecutableObject {

    private static final long serialVersionUID = 1L;

    private STArray mTemporaries;

    STSymbol mComment;
    STSymbol mCategory;
    STSymbol mPrimitiveName = null;
    
    protected STMethod() {
        mTemporaries = STArray.create();
    }
    
    public static STMethod create() {
        STMethod obj = new STMethod();
        obj.setClassProvider(new BindingClassProvider(obj) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Method;
            }
        });
        
        return obj;
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
