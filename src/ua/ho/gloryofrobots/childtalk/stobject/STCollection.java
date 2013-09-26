package ua.ho.gloryofrobots.childtalk.stobject;

public class STCollection extends STObject {
    public interface ForeachFunction {
        // must return false to stop foreach
        public boolean call(STObject obj);
    }

    private static final long serialVersionUID = 2412097950932247506L;

    private STObject[] mData;
    protected static final int DEFAULT_SIZE = 20;
    
    public static STCollection create(int size, STClass klass) {
        STCollection collection = new STCollection(size);
        collection.setSTClass(klass);
        return collection;
    }
    
    protected STCollection() {
        mData = new STObject[DEFAULT_SIZE];
    }

    protected STCollection(int size) {
        mData = new STObject[size];
    }

    public int size() {
        return mData.length;
    }

    public STObject at(int position) {
        return mData[position];
    }

    public void put(int position, STObject value) {
        while (position >= mData.length) {
            grow();
        }

        mData[position] = value;
    }

    private void grow() {
        int newSize = size() + DEFAULT_SIZE;
        growTo(newSize);
    }
    
    public void growTo(int newSize) {
        STObject[] newData = new STObject[newSize];
        System.arraycopy(mData, 0, newData, 0, mData.length);
        mData = null;
        mData = newData;
    }
    
    public boolean has(STObject key) {
        for (STObject obj : mData) {
            if (obj == key) {
                return true;
            }
        }

        return false;
    }

    public void clear() {
        mData = new STObject[mData.length];
    }

    public int indexOf(STObject obj) {
        int size = size();
        for (int i = 0; i < size; i++) {
            if (mData[i] == obj) {
                return i;
            }
        }
        return -1;
    }

    public void foreach(ForeachFunction fn) {
        int size = size();
        for (int i = 0; i < size; i++) {
            if (fn.call(mData[i]) == false) {
                return;
            }
        }
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append("<Collection " + getClass().getSimpleName() + " ");
        foreach(new ForeachFunction() {

            @Override
            public boolean call(STObject obj) {
                if(obj == null) return false;
                
                builder.append(obj.toString() + ", ");
                return true;
            }
        });
        builder.append(">");
        return builder.toString();
    }
}
