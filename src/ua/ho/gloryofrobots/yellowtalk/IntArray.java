package ua.ho.gloryofrobots.yellowtalk;

public class IntArray {
    protected int[] mCode;
    protected int mSize;
    protected final int DEFAULT_SIZE = 50;
    
    
    public IntArray() {
        mCode = new int[DEFAULT_SIZE];
    }

    public int getCountSettedElements() {
        return mSize;
    }
    
    public int getActualArraySize() {
        return mCode.length;
    }

    public int get(int index) {
        return mCode[index];
    }

    public void set(int index, int value) {
        while(index >= mCode.length) {
            grow();
        }
        
        mCode[index] = value;
        
        if(index > mSize - 1) {
            mSize = index + 1;
        }
    }
    
    private void grow() {
        int newSize = getCountSettedElements() + DEFAULT_SIZE;
        int [] newData = new int[newSize];
        System.arraycopy(mCode, 0, newData, 0, mCode.length);
        mCode = null;
        mCode = newData;
    }
}
