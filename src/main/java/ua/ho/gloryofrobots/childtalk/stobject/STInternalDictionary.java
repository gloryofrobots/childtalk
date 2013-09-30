package ua.ho.gloryofrobots.childtalk.stobject;

import java.util.ArrayList;
import java.util.HashMap;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STInternalDictionary extends STObject {

    private static final long serialVersionUID = 1L;

    protected HashMap<STObject, Integer> mBinding;
    protected ArrayList<STObject> mData;
    
    protected STInternalDictionary() {
        super();
        mBinding = new HashMap<STObject, Integer>();
        mData = new ArrayList<STObject>();
    }

    public static STInternalDictionary create() {
        STInternalDictionary obj = new STInternalDictionary();
        obj.setClassProvider(new BindingClassProvider(obj) {
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().InternalDictionary;
            }
        });
        return obj;
    }

    public STObject at(STObject name) {
        Integer index = mBinding.get(name);
        if (index == null) {
            return null;
        }
        return mData.get(index);
    }

    public boolean has(STObject name) {
        return mBinding.containsKey(name);
    }

    public void put(STObject name, STObject object) {
        mData.add(object);
        int index = mData.size() - 1;
        mBinding.put(name, index);
    }

    public STObject at(int index) {
        return mData.get(index);
    }

    public int getIndex(STObject name) {
        Integer index = mBinding.get(name);
        return index;
    }
}
