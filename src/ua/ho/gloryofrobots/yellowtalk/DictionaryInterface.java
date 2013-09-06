package ua.ho.gloryofrobots.yellowtalk;

import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public interface DictionaryInterface<K,V> {
    V at(K name);

    public boolean has(K name);
    
    void put(K name, V object);
}
