package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.compilation.CompileInfo;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface;

public abstract class ExecutableNode extends Node {
    protected CompileInfo mCompileInfo;
    protected List<String> mArguments;
    protected List<String> mTemporaries;
    BodyNode mBody;
    
    ExecutableNode() {
        mCompileInfo = new CompileInfo();
        mTemporaries = new ArrayList<String>();
        mBody = new BodyNode();
        mArguments = new ArrayList<String>();
        
    }
    
    public List<String> getArguments() {
        return mArguments;
    }

    public void addArgument(String argument) {
        mArguments.add(argument);
    }

    public void addTemporary(String temporary) {
        mTemporaries.add(temporary);
    }

    public List<String> getTemporaries() {
        return mTemporaries;
    }
    
    public void initCompileInfo(int startPosition, int endPosition, ProgramTextStreamInterface stream) {
        mCompileInfo.intitialise(startPosition, endPosition, stream);
    }

    public CompileInfo getCompileInfo() {
        return mCompileInfo;
    }
    
    public BodyNode getBody() {
        return mBody;
    }
}
