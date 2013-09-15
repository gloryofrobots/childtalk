package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.StackInterface;
import ua.ho.gloryofrobots.yellowtalk.Universe;

public class STStack extends STObject implements StackInterface<STObject> {
    private static final long serialVersionUID = 1L;
    private STObject[] mData;
    private int mIndex;
    private static final int DEFAULT_SIZE = 20;

    protected STStack() {
        mData = new STObject[DEFAULT_SIZE];
        mIndex = 0;
    }

    public static STStack create() {
        STStack obj = new STStack();
        obj.setSTClass(Universe.classes().Stack);
        return obj;
    }

    public void push(STObject obj) {
        mData[mIndex] = obj;
        mIndex++;
        if (mIndex >= mData.length) {
            grow();
        }
    }

    private void grow() {
        // TODO Auto-generated method stub
        int newSize = mData.length + mData.length / 2;
        STObject[] newData = new STObject[newSize];
        System.arraycopy(mData, 0, newData, 0, mData.length);
        mData = newData;
    }

    public STObject pop() {
        mIndex -= 1;
        STObject obj = mData[mIndex];
        return obj;
    }

    public STObject peek() {
        if (mIndex == 0) {
            return null;
        }

        return mData[getCurrentIndex()];
    }
    
    public int getCurrentIndex() {
        return mIndex - 1;
    }
    
    public int getCurrentPosition() {
        return mIndex;
    }

    public void setIndex(int position) {
        mIndex = position;
    }

    public void set(int position, STObject returnValue) {
        mData[position] = returnValue;
    }

    public STObject getFromEnd(int shift) {
        return mData[mIndex - shift];
    }

}
