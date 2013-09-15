package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.DictionaryInterface;
import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STDictionary extends STObject implements DictionaryInterface<STObject, STObject> {
    private static final long serialVersionUID = 1L;
    protected HashMap<STObject, STObject> mData;
    
    public static STDictionary create() {
        STDictionary obj = new STDictionary();
        obj.setClassProvider(new BindingClassProvider(obj) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Dictionary;
            }
        });
        
        return obj;
    }
    
    private STDictionary() {
        mData = new HashMap<STObject, STObject>();
    }
    
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
        return mData.get(name);
    }

    @Override
    public boolean has(STObject name) {
        return mData.containsKey(name);
    }

    @Override
    public void put(STObject name, STObject object) {
        mData.put(name, object);        
    }

}
