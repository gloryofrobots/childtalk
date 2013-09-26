package ua.ho.gloryofrobots.childtalk;

public interface InternalDictionaryInterface<K,V> extends DictionaryInterface<K,V> {
    V at(int index);
    int getIndex(K name);
}
