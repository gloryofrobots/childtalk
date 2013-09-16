package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.compilation.CompileInfo;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.compilation.Token;
import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class BlockNode extends ExecutableNode implements NodeFactory{
    protected BodyNode mBody;
    protected List<String> mArguments;
    
    public BlockNode() {   
        mBody = new BodyNode();
        mArguments = new ArrayList<String>();
    }
    
    public List<String> getArguments() {
        return mArguments;
    }

    public void addArgument(String argument) {
        mArguments.add(argument);
    }
    
    public BodyNode getBody() {
        return mBody;
    }

    
    @Override
    void writeRepresentation(StringWriter writer) {
        writer.write("[");
        
        if(mArguments.size() > 0){
           for(String arg: mArguments) {
               writer.write(":%s ", arg);
           }
           
           writer.write("| ");
        }
        
        writer.write("%s", mBody);
        
        writer.write("]");
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
                block.addArgument(STSymbol.unique(varName));
            } catch (DuplicateVariableException e) {
                throw new NodeFactoryException("Duplicate block argument " + varName);
            }
        }
        
        return block;
    }
}
