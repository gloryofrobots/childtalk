package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

public class BlockNode extends Node {
    public BlockNode() {
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
    protected BodyNode mBody = new BodyNode();
    protected List<String> mArguments;
    
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
}
