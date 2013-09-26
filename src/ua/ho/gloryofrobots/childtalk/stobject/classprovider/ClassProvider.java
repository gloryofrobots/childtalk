package ua.ho.gloryofrobots.childtalk.stobject.classprovider;

import ua.ho.gloryofrobots.childtalk.stobject.STClass;

//We use ClassProvider for later binding default types with ST classes
public interface ClassProvider {
    public STClass getSTClass();
}
