package ua.ho.gloryofrobots.childtalk.node;

import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public interface NodeFactory {
    public STObject
    createObject() throws NodeFactoryException;
}
