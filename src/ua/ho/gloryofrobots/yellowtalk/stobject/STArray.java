package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STArray extends STObject {

    private static final long serialVersionUID = 1L;
    private static final int DEFAULT_SIZE = 5;
    
    ArrayList<STObject> mElements;
    
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
    protected STArray(int size) {
        mElements = new ArrayList<STObject>(size);
        for (int i = 0; i < size; i++) {
            mElements.add(Universe.objects().NIL);
        }
    }
    
    protected STArray() {
        mElements = new ArrayList<STObject>();
    }
    
    public int size() {
        return mElements.size();
    }
    
    public void add(STObject object) {
        mElements.add(object);
    }
    
    public void set(int index, STObject object) {
        mElements.set(index, object);
    }
    
    @SuppressWarnings("unchecked")
    public <T extends STObject>
    T getAndCast(int index) {
        STObject value = get(index);
        T obj = null;
        try {
            obj = (T) value;    
        } catch(ClassCastException e) {
            return null;
        } 
        
        return obj;
    }
    
    public boolean has(STObject obj) {
        return mElements.contains(obj);
    }
    
    public STObject get(int index) {
        return mElements.get(index);
    }
    
    public STObject last() {
        return get(mElements.size() - 1);
    }
    
    public STObject first() {
        return get(0);
    }

    public int indexOf(STObject obj) {
        return mElements.indexOf(obj);
    }

    public void clear() {
        mElements.clear();
    }

    
    
}
