package ua.ho.gloryofrobots.yellowtalk.stobject.classprovider;

import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;

//We use ClassProvider for later binding default types with ST classes
public interface ClassProvider {
    public STClass getSTClass();
}
