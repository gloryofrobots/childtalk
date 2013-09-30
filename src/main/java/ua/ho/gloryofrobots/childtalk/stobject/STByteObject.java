package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STByteObject extends STObject {

    private static final long serialVersionUID = 1L;

    private byte[] mData;

    static public STByteObject create(int size) {
        STByteObject obj = new STByteObject(size);
        obj.setClassProvider(new BindingClassProvider(obj) {

            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().ByteArray;
            }
        });
        return obj;
    }

    static public STByteObject create(byte[] data) {
        STByteObject obj = new STByteObject(data);
        obj.setClassProvider(new BindingClassProvider(obj) {

            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().ByteArray;
            }
        });
        return obj;
    }

    protected STByteObject() {
        mData = new byte[10];
    }

    protected STByteObject(int size) {
        mData = new byte[size];
    }

    protected STByteObject(byte[] data) {
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

    public byte[] getBytes() {
        return mData;
    }

    public void growTo(int newSize) {
        if (newSize < size()) {
            return;
        }

        byte[] newData = new byte[newSize];
        System.arraycopy(mData, 0, newData, 0, mData.length);
        mData = null;
        mData = newData;
    }

    public long getHash() {
        long hash = 5381;
        for (int i = 0; i < mData.length; i++) {
            if (mData[i] == 0) {
                break;
            }
            hash = ((hash << 5) + hash) + mData[i];
        }

        return hash;
    }

    public char[] toCharArray() {
        return new String(mData).toCharArray();
    }

    public STByteObject merge(STByteObject second) {
        byte[] data = second.getBytes();
        int size = data.length + mData.length;
        byte[] newData = new byte[size];
        System.arraycopy(mData, 0, newData, 0, mData.length);
        System.arraycopy(data, 0, newData, mData.length, data.length);

        return STByteObject.create(newData);
    }
}
