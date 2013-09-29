package ua.ho.gloryofrobots.childtalk.node;

import java.util.List;

import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.stobject.STBlock;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class BlockNode extends ExecutableNode implements NodeFactory {
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("[");

        if (mArguments.size() > 0) {
            for (String arg : mArguments) {
                writer.write(":%s ", arg);
            }

            writer.writeln("| ");
        }

        if (mTemporaries.size() > 0) {
            writer.write("| ");
            for (String arg : mTemporaries) {
                writer.write("%s ", arg);
            }

            writer.write("| ");
        }
        
        writer.write("%s", mBody);

        writer.write(" ]");
    }

    public void onAccept(Visitor visitor) {
        visitor.visit(this);
    }

    @Override
    public STObject createObject() throws NodeFactoryException {
        STBlock block = STBlock.create();

        block.setCompileInfo(mCompileInfo);

        List<String> arguments = getArguments();

        for (String varName : arguments) {
            try {
                block.addArgument(STSymbol.create(varName));
            } catch (DuplicateVariableException e) {
                throw new NodeFactoryException("Duplicate block argument "
                        + varName);
            }
        }
        
        List<String> temporaries = getTemporaries();
        for (String varName : temporaries) {
            try {
                block.addTemporary(STSymbol.create(varName));
            } catch (DuplicateVariableException e) {
                throw new NodeFactoryException("Duplicate method temporary variable " + varName);
                
            }
        }
        return block;
    }
}
