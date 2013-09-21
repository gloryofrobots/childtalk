package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
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
        mInstanceVariables = STArray.create(10);
        mPrimitives = STDictionary.create();
        mSubclasses = STArray.create();
        transformToScopedObject();
        mScope.put(STSymbol.unique("classSubclasses"), mSubclasses);
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
        mScope.put(STSymbol.unique("classSuperclass"), mSuperClass);
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
        
        STClass th = this;
        
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
        mScope.put(STSymbol.unique("className"), mName);
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
        method.setOwnerClass(this);
    }

    public void addMethod(STSymbol selector, STMethod method) {
        mScope.put(selector, method);
        method.setOwnerClass(this);
    }
    
    public String toString() {
        return "<STClass  " + mName + ">";
    }

    public List<STSymbol> getUnknownPrimitives() {
        List<STSymbol> primitives = new ArrayList<STSymbol>();
        List<STObject> objects = mScope.asList();
        for(STObject obj : objects) {
            if((obj instanceof STMethod) == false) {
                continue;
            }
            
            STMethod method = (STMethod) obj;
            if(method.hasPrimitive() == false) {
                continue;
            }
            if(method.getPrimitive() == null) {
                primitives.add(method.getPrimitiveName());
            }
        }
        return primitives;
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
                scope.put(varName, Universe.objects().NIL);
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
}
