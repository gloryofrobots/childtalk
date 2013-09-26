package ua.ho.gloryofrobots.childtalk.stobject;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.childtalk.Universe;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STCollection.ForeachFunction;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STArray extends STCollection {

    private static final long serialVersionUID = 1L;
    private int mMaxIndex;
    
    public static STArray create() {
        STArray array  = new STArray();
        array.setClassProvider(new BindingClassProvider(array) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Array;
            }
        });
        return array;
    }
   
    public static STArray create(int size) {
        STArray array  = new STArray(size);
        array.setClassProvider(new BindingClassProvider(array) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Array;
            }
        });
        return array;
    }
    
    //TODO change to ARRAY
    private STArray(int size) {
        super(size);
    }
    
    protected STArray() {
         super();
    }
    
    public int getMaxSettedIndex() {
        return mMaxIndex;
    }
    
    //redefine size to show only setted elements
    public int size() {
        return getMaxSettedIndex();
    }
    
    public int capacity() {
        return super.size();
    }
    
    public void put(int position, STObject value) {
        super.put(position, value);
        if(position > mMaxIndex - 1) {
            mMaxIndex = position + 1;
        }
    }
    
    public void add(STObject object) {
        if(object == null) {
            SignalSuite.error("Set null object to array");
        }
        
        put(mMaxIndex, object);
    }
    
    @SuppressWarnings("unchecked")
    public <T extends STObject>
    T getAndCast(int index) {
        STObject value = at(index);
        T obj = null;
        try {
            obj = (T) value;    
        } catch(ClassCastException e) {
            return null;
        } 
        
        return obj;
    }
    
    public STObject last() {
        return at(getMaxSettedIndex() - 1);
    }
    
    public STObject first() {
        return at(0);
    }
    
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append("{");
        foreach(new ForeachFunction() {
            
            @Override
            public boolean call(STObject obj) {
                builder.append(obj.toString() + ", ");
                return true;
            }
        });
        builder.append("}");
        return builder.toString();
    }
}
