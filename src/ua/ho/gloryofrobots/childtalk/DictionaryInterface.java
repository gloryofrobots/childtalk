package ua.ho.gloryofrobots.childtalk;

import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public interface DictionaryInterface<K,V> {
    V at(K name);

    public boolean has(K name);
    
    void put(K name, V object);
}
