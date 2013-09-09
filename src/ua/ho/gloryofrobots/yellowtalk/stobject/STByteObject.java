package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;

public class STByteObject extends STObject {

    private static final long serialVersionUID = 1L;

    private byte[] mData;

    static public STByteObject create(int size) {
        STByteObject obj = new STByteObject(size);
        obj.setSTClass(Universe.classes().ByteArray);
        return obj;
    }
    
    static public STByteObject create(byte [] data) {
        STByteObject obj = new STByteObject(data);
        obj.setSTClass(Universe.classes().ByteArray);
        return obj;
    }
    
    protected STByteObject(int size) {
        mData = new byte[size];
    }
    
    protected STByteObject(byte [] data) {
        mData = data;
    }
    
    public void put(int index, byte value) {
        mData[index] = value;
    }

    public byte at(int index) {
        return mData[index];
    }

    public int size() {
        return mData.length;
    }
    
    public String toString() {
        return new String(mData);
    }
}
