package ua.ho.gloryofrobots.childtalk.node;

import java.util.List;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.stobject.STMethod;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class EvalNode extends ExecutableNode implements NodeFactory{

    @Override
    public STObject createObject() throws NodeFactoryException {
        //represent Eval as method
        STMethod method = STMethod.create();
        method.setSelector(STSymbol.create("Eval"));
        method.setCompileInfo(mCompileInfo);
        method.setOwnerClass(ImageSuite.image().classes().UndefinedObject);
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
