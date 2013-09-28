package ua.ho.gloryofrobots.childtalk;

import java.io.Serializable;

public class IntArray implements Serializable {
   
    private static final long serialVersionUID = 1L;
    
    protected int[] mData;
    protected int mMaxIndex;
    protected final int DEFAULT_SIZE = 50;
    
    
    public IntArray() {
        mData = new int[DEFAULT_SIZE];
    }

    public int getMaxSettedIndex() {
        return mMaxIndex;
    }
    
    public int getActualArraySize() {
        return mData.length;
    }

    public int get(int index) {
        return mData[index];
    }

    public void set(int index, int value) {
        while(index >= mData.length) {
            grow();
        }
        
        mData[index] = value;
        
        if(index > mMaxIndex - 1) {
            mMaxIndex = index + 1;
        }
    }
    
    private void grow() {
        int newSize = getMaxSettedIndex() + DEFAULT_SIZE;
        int [] newData = new int[newSize];
        System.arraycopy(mData, 0, newData, 0, mData.length);
        mData = null;
        mData = newData;
    }
}
