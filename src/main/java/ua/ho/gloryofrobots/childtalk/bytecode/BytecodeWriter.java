package ua.ho.gloryofrobots.childtalk.bytecode;


import ua.ho.gloryofrobots.childtalk.compilation.CompileInfo;
import ua.ho.gloryofrobots.childtalk.node.Node;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;

public class BytecodeWriter{
  
    private BytecodeArray mBytecode;
    private int mIndex;
    private CompileInfo mCompileInfo;

    public BytecodeWriter(STExecutableObject object) {
        mBytecode = object.getBytecode();
        mCompileInfo = object.getCompileInfo();
        mIndex = 0;
    }
    
    public void append(BytecodeType codeType, int argument, Node node) {
        short command = (short) codeType.ordinal();
        mBytecode.set(mIndex, command, argument);
        
        int position = CompileInfo.UNKNOWN_POSITION;
        if(node != null) {
            position = node.getPosition();
        }
        
        mCompileInfo.setPosition(mIndex, position);
        mIndex++;
    }
    
    public void pushConstant(BytecodeType.Constant constant, Node node) {
        append(BytecodeType.PUSH_CONSTANT, constant.ordinal(), node);
    }

    public BytecodeType getCodeFromEnd(int count) {
        int code = mBytecode.getHigh(mIndex - count);
        return BytecodeType.values()[code];
    }
}