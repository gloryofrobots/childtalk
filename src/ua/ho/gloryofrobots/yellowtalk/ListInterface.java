package ua.ho.gloryofrobots.yellowtalk;

import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public interface ListInterface extends Iterable<STObject>{
    <T extends STObject> 
    void append(T element);
    
    <T extends STObject> 
    T get();
}
