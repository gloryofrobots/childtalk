package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.DictionaryInterface;

public class STDictionary extends STObject implements DictionaryInterface<STObject, STObject> {
    private static final long serialVersionUID = 1L;
    protected HashMap<STObject, STObject> mBinding;
    
    @SuppressWarnings (value="unchecked")
    public <T extends STObject> 
    T getAndCast(STObject key) {
        STObject value = at(key);
        T obj = null;
        try {
            obj = (T) value;    
        } catch(ClassCastException e) {
            return null;
        } 
        
        return obj;
    }
    
    @Override
    public STObject at(STObject name) {
        return mBinding.get(name);
    }

    @Override
    public boolean has(STObject name) {
        return mBinding.containsKey(name);
    }

    @Override
    public void put(STObject name, STObject object) {
        mBinding.put(name, object);        
    }

}
