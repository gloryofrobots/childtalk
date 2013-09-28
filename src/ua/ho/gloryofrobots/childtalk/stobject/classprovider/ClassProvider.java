package ua.ho.gloryofrobots.childtalk.stobject.classprovider;

import java.io.Serializable;

import ua.ho.gloryofrobots.childtalk.stobject.STClass;

//We use ClassProvider for later binding default types with ST classes
public interface ClassProvider extends Serializable {
    public STClass getSTClass();
}
