package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public interface NodeFactory {
    public STObject
    createObject() throws NodeFactoryException;
}
