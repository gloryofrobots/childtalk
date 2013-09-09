package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;

public class STClass extends STObject {
    STArray mInstanceVariables;
    
    private static final long serialVersionUID = 1L;
    protected STScope mScope = new STScope();
    
    STSymbol mComment;
    STSymbol mCategory;
    STSymbol mName;
    STClass mSuperClass;
    STDictionary mPrimitives;
    STArray mSubclasses;
    Class<?> mInternalObjectClass = null;
    
    protected STClass() {
        mInstanceVariables = new STArray(10);
        mPrimitives = new STDictionary();
        mSubclasses = new STArray();
    }
    
    public void setInternalObjectClass(Class<?> _class) {
        mInternalObjectClass = _class;
    }
    
    public Class<?> getInternalObjectClass() {
        return mInternalObjectClass;
    }
    
    public static STClass create(String name) {
        STClass klass = new STClass();
        klass.setName(STSymbol.unique(name));
        return klass;
    }
    
    public void setSuperClass(STClass _class) {
        mSuperClass = _class;
    }
    
    public STClass getSuperClass() {
        return mSuperClass;
    }
    
    public STMethod findMethod(STObject selector) {
        return mScope.getAndCast(selector);
    }

    public void addInstanceVariable(STSymbol varName)
            throws DuplicateVariableException {
        if (mInstanceVariables.has(varName)) {
            throw new DuplicateVariableException(varName.toString());
        }

        mInstanceVariables.add(varName);
    }

    public void addClassVariable(STSymbol varName)
            throws DuplicateVariableException {
        getScope().putUnique(varName, null);
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
        return mName;
    }
    
    public void setPrimitive(STSymbol name, STPrimitive primitive) {
        if(mPrimitives.has(name) ) {
            throw new RuntimeException("Duplicated primitive " + name.toString());
        }
        
        mPrimitives.put(name, primitive);
    }
    
    public void setPrimitive(String name, STPrimitive primitive) {
        STSymbol symbolName = STSymbol.unique(name);
        setPrimitive(symbolName, primitive);
   }
    
    public STPrimitive getPrimitive(STSymbol name) {
        STClass host = this;
        while(STObject.isNotNilOrNull(host)) {
            STPrimitive primitive = host.getPrimitive(name);
            if(primitive != null) {
                return primitive;
            }
            
            host = host.getSuperClass();
        }
        
         return null;
    }

    public void addSubclass(STClass klass) {
        //Link scopes
        STScope scope = klass.getScope();
        
        mScope.append(scope);
        
        mSubclasses.add(klass);
    }
}
