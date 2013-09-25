package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class EvalNode extends ExecutableNode implements NodeFactory{

    @Override
    public STObject createObject() throws NodeFactoryException {
        //represent Eval as method
        STMethod method = STMethod.create();
        method.setSelector(STSymbol.create("Eval"));
        method.setCompileInfo(mCompileInfo);
        List<String> temporaries = getTemporaries();
        for (String varName : temporaries) {
            try {
                method.addTemporary(STSymbol.create(varName));
            } catch (DuplicateVariableException e) {
                throw new NodeFactoryException(
                        "Duplicate method temporary variable " + varName);

            }
        }
        return method;
    }

    @Override
    void writeRepresentation(StringWriter writer) {
        mBody.writeRepresentation(writer);
    }

    @Override
    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }
    
}
