package ua.ho.gloryofrobots.yellowtalk.bytecode;

public class BytecodeArray {
    int [] mCode;
    protected int mSize;
    protected final int DEFAULT_CODE_SIZE = 50;
    public BytecodeArray() {
        mCode = new int[DEFAULT_CODE_SIZE];
    }
    
    public int getSize() {
        return mSize;
    }
    
    public int get(int index) {
        return mCode[index];
    }
    
    public int getHigh(int code) {
        return 0;
    }
    
    public int getLow(int code) {
        return 0;
    }
    
    public void set(int high, int low) {
        
    }
    
    public void append(BytecodeType codeType, int argument) {
        
    }
    
    public void pushConstant(BytecodeType.Constant constant) {
        append(BytecodeType.PUSH_CONSTANT,constant.ordinal());
        
    }
}
