package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Set;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.ClassProvider;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.DefaultClassProvider;


public class STObject implements Serializable {

    private static final long serialVersionUID = 1L;
    
    protected STScope mScope = null;
    
    private ClassProvider mClassProvider;
    
    @SuppressWarnings("unchecked")
    public static <T extends STObject> 
    T newObject(STClass _class, Class<T> aClass) {
        T object = null;
        try {
            
            Method constructor = aClass.getMethod("create", STClass.class);
            object = (T) constructor.invoke(null, _class);
            object.transformToScopedObject();
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
     
    public static STObject createEmpty() {
        STObject object = new STObject();
        return object;
    }
    
    public static STObject createWithClass(STClass _class) {
        STObject object = new STObject();
        object.transformToScopedObject();
        object.setSTClass(_class);
        return object;
    }
    
    protected STObject() {
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
    
    protected void transformToScopedObject() {
        mScope = STScope.create();
    }
    
    public STClass getSTClass() {
        //We use ClassProvider for later binding default types with ST classes
        STObject nil = Universe.objects().NIL;
        return mClassProvider.getSTClass();
    }
    
    public void setSTClass(STClass _class) {
        if(_class == null) {
            SignalSuite.error("STObject.setSTClass. class is null");
            return;
        }
        
        _class.initObject(this);
        
        mClassProvider = new DefaultClassProvider(_class);
        if(mScope == null) {
            return;
        }
        
        //STScope scope = _class.getScope();
        //mScope.append(scope);
    }
    
    public void setClassProvider(ClassProvider classProvider) {
        mClassProvider = classProvider;
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
   public String toString() {
       STClass klass = getSTClass();
       String data = String.format("<STObject at 0x%s ", Integer.toHexString(this.hashCode()));
       if(klass != null) {
           data += "of class " + klass.getName().toString();
       }
       data += ">";
       return data;
   }
   
   public STObject lookup(STObject selector) {
       STObject obj = null;
       if(mScope != null) {
           obj = mScope.lookup(selector);
       } 
       
       if(obj != null) {
           return obj;
       }
       
       STClass klass = getSTClass();
       if(klass == null) {
           return null;
       }
       
       return klass.lookup(selector);
   }
}



