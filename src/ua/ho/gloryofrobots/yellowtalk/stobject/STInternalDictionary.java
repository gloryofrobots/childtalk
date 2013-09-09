package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.ArrayList;
import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.InternalDictionaryInterface;

public class STInternalDictionary extends STObject implements
        InternalDictionaryInterface<STObject, STObject> {
    
    private static final long serialVersionUID = 1L;

    protected HashMap<STObject, Integer> mBinding;
    protected ArrayList<STObject> mData;
    
    @Override
    public STObject at(STObject name) {
        Integer index = mBinding.get(name);
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
