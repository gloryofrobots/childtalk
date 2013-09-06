package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.InternalDictionaryInterface;

public class STInternalDictionary extends STObject implements
        InternalDictionaryInterface<STObject, STObject> {

    private static final long serialVersionUID = 1L;

    protected HashMap<STObject, STObject> mData;

    @Override
    public STObject at(STObject name) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean has(STObject name) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void put(STObject name, STObject object) {
        // TODO Auto-generated method stub

    }

    @Override
    public STObject at(int index) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int getIndex(STObject name) {
        // TODO Auto-generated method stub
        return 0;
    }

}
