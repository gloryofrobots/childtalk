package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.IntArray;

public class BytecodeArray extends IntArray{
    public short getHigh(int index) {
        return BytecodeUtils.unpackCommand(mData[index]);
    }

    public int getLow(int index) {
        return BytecodeUtils.unpackArgument(mData[index]);
    }

    public void set(int index, short high, int low) {
        set(index, BytecodeUtils.pack(high, low));
    }
    
    public String toString() {
        String data = "[";
        int size = getMaxSettedIndex();
        for(int i = 0; i < size; i++) {
            int low = getLow(i);
            short high  = getHigh(i);
            data += BytecodeType.values()[high].toString() + " : " + new Integer(low).toString() + ",";
        }
        data += "]";
        return data;
    }
}
