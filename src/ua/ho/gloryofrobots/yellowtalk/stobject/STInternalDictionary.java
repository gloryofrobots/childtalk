package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;
import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.InternalDictionaryInterface;
import ua.ho.gloryofrobots.yellowtalk.Universe;

public class STInternalDictionary extends STObject implements
        InternalDictionaryInterface<STObject, STObject> {
    
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
        return obj;
    }
    
    @Override
    public STObject at(STObject name) {
        Integer index = mBinding.get(name);
        if(index == null) {
            return null;
        }
        return mData.get(index);
    }

    @Override
    public boolean has(STObject name) {
        return mBinding.containsKey(name);
    }

    @Override
    public void put(STObject name, STObject object) {
        mData.add(object);
        int index = mData.size() - 1;
        mBinding.put(name, index);
    }

    @Override
    public STObject at(int index) {
        return mData.get(index);
    }

    @Override
    public int getIndex(STObject name) {
        Integer index = mBinding.get(name);
        return index;
    }
}
