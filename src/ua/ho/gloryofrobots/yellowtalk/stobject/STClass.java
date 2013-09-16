package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STClass extends STObject {
    STArray mInstanceVariables;

    private static final long serialVersionUID = 1L;

    STSymbol mComment;
    STSymbol mCategory;
    STSymbol mName;
    STClass mSuperClass;
    STDictionary mPrimitives;
    STArray mSubclasses;
    Class<?> mInternalObjectClass = null;

    public static STClass create(String name) {
        return create(STSymbol.unique(name));
    }

    public static STClass create(STSymbol name) {
        STClass klass = new STClass();
        klass.setName(name);
        klass.setClassProvider(new BindingClassProvider(klass) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Class;
            }
        });
        return klass;
    }
    
    protected STClass() {
        mInstanceVariables = new STArray(10);
        mPrimitives = STDictionary.create();
        mSubclasses = STArray.create();
        transformToScopedObject();
        mScope.put(STSymbol.unique("subclasses"), mSubclasses);

        mScope.put(STSymbol.unique("superclass"), mSuperClass);
        mScope.put(STSymbol.unique("instanceVariableNames"), mInstanceVariables);
    }

    public void setInternalObjectClass(Class<?> _class) {
        mInternalObjectClass = _class;
    }

    public Class<?> getInternalObjectClass() {
        return mInternalObjectClass;
    }

    public void setSuperClass(STClass _class) {
        mSuperClass = _class;
    }

    public STClass getSuperClass() {
        return mSuperClass;
    }
    
    public STMethod findMethodInSuperclass(STObject selector) {
        STMethod method =  (STMethod) mScope.lookup(selector);
        
        if(method == null && mSuperClass != null) {
            method =  (STMethod) mSuperClass.findMethodInSuperclass(selector);
        }
        
        return method;
    }
    
    public STMethod findMethod(STObject selector) {
        STMethod method = findMethodInSuperclass(selector);
        STClass th = this;
        
        STClass stclass = getSTClass();
        if(method == null) {
            if(stclass == null) {
                return method;
            }
            method =  stclass.findMethodInSuperclass(selector);
        }

        return method;
    }
    
    public STMethod findVariable(STObject selector) {
        STMethod method =  (STMethod) mScope.lookup(selector);
        
        if(method == null && mSuperClass != null) {
            method =  (STMethod) mSuperClass.findVariable(selector);
        }
        
        return method;
    }
    
    public void addInstanceVariable(STSymbol varName)
            throws DuplicateVariableException {
        if (mInstanceVariables.has(varName)) {
            throw new DuplicateVariableException(varName.toString());
        }

        mInstanceVariables.add(varName);
    }
    
    public void addInstanceVariables(STArray vars)
            throws DuplicateVariableException {
        int size = vars.size();
        for(int i = 0; i < size; i++) {
            STSymbol varName = vars.getAndCast(i);
            addInstanceVariable(varName);
        }
    }
    
    public void addClassVariable(STSymbol varName)
            throws DuplicateVariableException {
        getScope().putUnique(varName, null);
    }
    
    public void addClassVariables(STArray vars)
            throws DuplicateVariableException {
        int size = vars.size();
        for(int i = 0; i < size; i++) {
            STSymbol varName = vars.getAndCast(i);
            addClassVariable(varName);
        }
    }
    
    

    public void setComment(STSymbol comment) {
        mComment = comment;
    }

    public void setCategory(STSymbol category) {
        mCategory = category;
    }

    public void setName(STSymbol name) {
        mName = name;
        mScope.put(STSymbol.unique("name"), mName);
    }

    public STSymbol getName() {
        return mName;
    }

    public void setPrimitive(STSymbol name, STPrimitive primitive) {
        if (mPrimitives.has(name)) {
            //TODO signal
            throw new RuntimeException("Duplicated primitive "
                    + name.toString());
        }

        mPrimitives.put(name, primitive);
    }

    public void setPrimitive(String name, STPrimitive primitive) {
        STSymbol symbolName = STSymbol.unique(name);
        setPrimitive(symbolName, primitive);
    }

    public STPrimitive getPrimitive(STSymbol name) {
        STClass host = this;
        while (STObject.isNotNilOrNull(host)) {
            STPrimitive primitive = host._getPrimitive(name);
            if (primitive != null) {
                return primitive;
            }

            host = host.getSuperClass();
        }

        return null;
    }

    private STPrimitive _getPrimitive(STSymbol name) {
        STObject result = mPrimitives.at(name);
        if(result == null) {
            return null;
        }
        
        return result.castToSubclass();
    }

    public void addSubclass(STClass klass) {
        // Link scopes
        /*STScope scope = klass.getScope();
        
        scope.append(mScope);*/

        mSubclasses.add(klass);
    }

    public void addStaticMethod(STSymbol selector, STMethod method) {
        getSTClass().addMethod(selector, method);
    }

    public void addMethod(STSymbol selector, STMethod method) {
        mScope.put(selector, method);
    }
    
    public String toString() {
        return "<STClass  " + mName + ">";
    }
}
