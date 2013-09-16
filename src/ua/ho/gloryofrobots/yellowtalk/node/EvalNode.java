package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public class EvalNode extends BodyNode implements NodeFactory{

    @Override
    public STObject createObject() throws NodeFactoryException {
        //represent Eval as method
        STMethod method = STMethod.create();
        method.setCompileInfo(mCompileInfo);
        return method;
    }
    
}
