package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Set;

import ua.ho.gloryofrobots.yellowtalk.Universe;

/*
Array.cpp
BlockClosure.cpp
BlockContext.cpp
Boolean.cpp
ByteArray.cpp
Character.cpp
ClassObject.cpp
CompiledBlock.cpp
CompiledMethod.cpp
Context.cpp
Dictionary.cpp
Float.cpp
Globals.cpp
LargeInteger.cpp
MetaClass.cpp
MethodContext.cpp
NilObject.cpp
Object.cpp
Process.cpp
Semaphore.cpp
SmallInteger.cpp
String.cpp
Symbol.cpp
Symbols.cpp
VariableBinding.cpp*/


public class STObject implements Serializable {
    private static final long serialVersionUID = 1L;
    protected STScope mScope = null;
    
    private STClass mClass;
    
    public static <T extends STObject> 
    T newObject(STClass _class, Class<T> aClass) {
        T object = null;
        try {
            
            Constructor<T> constructor = aClass.getConstructor();
            object = constructor.newInstance();
            object.setSTClass(_class);
        } catch (InstantiationException e) {
            return null;
        } catch (IllegalAccessException e) {
            return null;
        } catch (NoSuchMethodException e) {
            return null;
        } catch (SecurityException e) {
            return null;
        } catch (IllegalArgumentException e) {
            return null;
        } catch (InvocationTargetException e) {
            return null;
        } 
        return object;
    }
    
    public static <T extends STObject> 
    T newObject(STClass _class, Class<T> aClass, STObject argument) {
        T object = null;
        try {
            Constructor<T> constructor = aClass.getDeclaredConstructor(new Class[] {STObject.class});
            object = constructor.newInstance(argument);
            object.setSTClass(_class);
        } catch (InstantiationException e) {
            return null;
        } catch (IllegalAccessException e) {
            return null;
        } catch (NoSuchMethodException e) {
            return null;
        } catch (SecurityException e) {
            return null;
        } catch (IllegalArgumentException e) {
            return null;
        } catch (InvocationTargetException e) {
            return null;
        } 
        return object;
    }
    
    public STObject shallowCopy() {
        try {
            return (STObject)this.clone();
        } catch (CloneNotSupportedException e) {
            return Universe.objects().NIL;
        }
    }
     
    public STScope getScope() {
        return mScope;
    }
    
    public void setScope(STScope scope) {
        mScope = scope;
    }
    
    public STClass getSTClass() {
        return mClass;
    }
    
    public void setSTClass(STClass _class) {
        //TODO remove
        if(_class == null) {
            return;
        }
        mClass = _class;
        STScope scope = mClass.getScope();
        scope.append(mScope);
    }
    
    public static boolean isNotNilOrNull(STObject object) {
        if(object == null || object == Universe.objects().NIL) {
            return false;
        }
        
        return true;
    }
    
    @SuppressWarnings("unchecked")
    public
    <T extends STObject>
    T castToSubclass() {
        try {
            return (T) this;    
        } catch(ClassCastException e) {
            //FIXME
            return null;
        } 
    }
   
}



