package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

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
    boolean mIsInitialised;
    
    public static STClass create(String name) {
        return create(STSymbol.create(name));
    }
    
    public boolean isInitialised() {
        return mIsInitialised;
    }
    
    public void setInitialised() {
        mIsInitialised = true;
    }
    
    public static STClass create(STSymbol name) {
        STClass klass = new STClass();
        klass.setName(name);
        klass.setClassProvider(new BindingClassProvider(klass) {
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().Class;
            }
        });
        return klass;
    }
    
    protected STClass() {
        mInstanceVariables = STArray.create(10);
        mPrimitives = STDictionary.create();
        mSubclasses = STArray.create();
        transformToScopedObject();
        mScope.put(STSymbol.create("classSubclasses"), mSubclasses);
        mScope.put(STSymbol.create("instanceVariableNames"), mInstanceVariables);
    }

    public void setInternalObjectClass(Class<?> _class) {
        mInternalObjectClass = _class;
    }

    public Class<?> getInternalObjectClass() {
        return mInternalObjectClass;
    }

    public void setSuperClass(STClass _class) {
        mSuperClass = _class;
        mScope.put(STSymbol.create("classSuperclass"), mSuperClass);
    }

    public STClass getSuperClass() {
        return mSuperClass;
    }
    
    protected STObject _lookup(STObject selector) {
        STObject obj = null;
        obj = mScope.lookup(selector);
       
        if(obj != null) {
            return obj;
        }
        
        if(mSuperClass != null) {
            obj =  mSuperClass._lookup(selector);
        }
        
        return obj;
    }
    
    public STObject lookup(STObject selector) {
        STObject obj = _lookup(selector);
        
        if(obj != null) {
            return obj;
        }
        
        STClass stclass = getSTClass();
        
        if(stclass == null) {
            return null;
        }
        
        obj =  stclass._lookup(selector);
        
        return obj;
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
        mScope.putUnique(varName, ImageSuite.image().objects().NIL);
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
        mScope.put(STSymbol.create("className"), mName);
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
        STSymbol symbolName = STSymbol.create(name);
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
        mSubclasses.add(klass);
    }

    public void addStaticMethod(STSymbol selector, STMethod method) {
        getSTClass().addMethod(selector, method);
        method.setOwnerClass(this);
    }

    public void addMethod(STSymbol selector, STMethod method) {
        mScope.put(selector, method);
        method.setOwnerClass(this);
    }
    
    public String toString() {
        return "<STClass  " + mName + ">";
    }

    public STExecutableObject findMethod(STObject selector) {
        STObject obj = lookup(selector);
        if(obj == null) {
            return null;
        }
        
        try {
            return (STMethod) obj;
        } catch(ClassCastException e) {
            return null;
        }
    }
    
    private void _initObject(final STObject object) {
       
        mInstanceVariables.foreach(new STCollection.ForeachFunction() {
            @Override
            public boolean call(STObject varName) {
                if(STObject.isNotNilOrNull(varName) == false) {
                    return false;
                }
                STScope scope = object.getScope();
                scope.put(varName, ImageSuite.image().objects().NIL);
                return true;
            }
        });
       
    }
    
    public void initObject(STObject object) {
        if(mSuperClass != null) {
            mSuperClass.initObject(object);
        }
        
        _initObject(object);
    }

    public void clear() {
        setSTClass(null);
        setSuperClass(null);
        setName(null);
        setCategory(null);
        setClassProvider(null);
        //mInstanceVariables = null;
        STScope scope = getScope();
        scope.setClassProvider(null);
        mSubclasses = null;
        mComment = null;
        mInternalObjectClass = null;
        mPrimitives = null;
    }
}
