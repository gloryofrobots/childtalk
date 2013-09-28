package ua.ho.gloryofrobots.childtalk.stobject;

import java.util.HashMap;

import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;
//TODO MAY be remove completely
public class STDictionary extends STObject {
    private static final long serialVersionUID = 1L;
    protected HashMap<STObject, STObject> mData;
    
    public static STDictionary create() {
        STDictionary obj = new STDictionary();
        obj.setClassProvider(new BindingClassProvider(obj) {
            @Override
            protected STClass _getSTClass() {
                return null;
                //return ImageSuite.image().classes().Dictionary;
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
    
    public STObject at(STObject name) {
        return mData.get(name);
    }

    public boolean has(STObject name) {
        return mData.containsKey(name);
    }

    public void put(STObject name, STObject object) {
        mData.put(name, object);        
    }

}
