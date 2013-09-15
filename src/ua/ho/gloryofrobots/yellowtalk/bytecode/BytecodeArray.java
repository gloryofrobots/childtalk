package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.IntArray;

public class BytecodeArray extends IntArray{
    public short getHigh(int index) {
        return BytecodeUtils.unpackCommand(mCode[index]);
    }

    public int getLow(int index) {
        return BytecodeUtils.unpackArgument(mCode[index]);
    }

    public void set(int index, short high, int low) {
        set(index, BytecodeUtils.pack(high, low));
    }
}
