package ua.ho.gloryofrobots.yellowtalk.node;

import ua.ho.gloryofrobots.yellowtalk.compilation.CompileInfo;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface;

public abstract class ExecutableNode extends Node {
    protected CompileInfo mCompileInfo;
    
    ExecutableNode() {
        mCompileInfo = new CompileInfo();
    }
    
    public void initCompileInfo(int startPosition, int endPosition, ProgramTextStreamInterface stream) {
        mCompileInfo.intitialise(startPosition, endPosition, stream);
    }

    public CompileInfo getCompileInfo() {
        return mCompileInfo;
    }
}
