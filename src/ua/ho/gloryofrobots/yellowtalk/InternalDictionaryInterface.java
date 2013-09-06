package ua.ho.gloryofrobots.yellowtalk;

public interface InternalDictionaryInterface<K,V> extends DictionaryInterface<K,V> {
    V at(int index);
    int getIndex(K name);
}
