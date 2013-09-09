package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;

public class STArray extends STObject {

    private static final long serialVersionUID = 1L;
    private static final int DEFAULT_SIZE = 5;
    
    ArrayList<STObject> mElements;
    
    public STArray() {
        this(DEFAULT_SIZE);
    }
    
    public STArray(int size) {
        mElements = new ArrayList<STObject>(size);
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

    
    
}
