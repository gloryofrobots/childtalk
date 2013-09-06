package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;
import java.util.List;

public class STArray extends STObject {

    private static final long serialVersionUID = 1L;
    
    List<STObject> mElements;
    
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
    
    public STObject get(int index) {
        return mElements.get(index);
    }
    
    public STObject last() {
        return get(mElements.size() - 1);
    }
    
    public STObject first() {
        return get(0);
    }
    
}
