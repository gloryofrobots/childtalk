package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public class EvalNode extends ExecutableNode implements NodeFactory{

    @Override
    public STObject createObject() throws NodeFactoryException {
        //represent Eval as method
        STMethod method = STMethod.create();
        method.setCompileInfo(mCompileInfo);
        return method;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        mBody.writeRepresentation(writer);
    }

    @Override
    public void onAccept(Visitor visitor) {
        visitor.visit(mBody);
    }
    
}
