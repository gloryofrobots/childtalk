package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.compilation.CompileInfo;

public class BytecodeWriter {
    private BytecodeArray mBytecode;
    private int mIndex;
    private CompileInfo mCompileInfo;

    public BytecodeWriter(BytecodeArray array, CompileInfo info) {
        mCompileInfo = info;
        mBytecode = array;
        mIndex = 0;
    }
    
    public void append(BytecodeType codeType, int argument, int position) {
        short command = (short) codeType.ordinal();
        mBytecode.set(mIndex, command, argument);
        mIndex++;
    }
    
    public void pushConstant(BytecodeType.Constant constant) {
        append(BytecodeType.PUSH_CONSTANT, constant.ordinal());
    }
}