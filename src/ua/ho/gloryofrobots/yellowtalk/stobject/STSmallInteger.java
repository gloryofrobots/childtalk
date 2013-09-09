package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;

public class STSmallInteger extends STNumber {

    private static final long serialVersionUID = 1L;

    private int mData;

    private STSmallInteger(int data) {
        mData = data;
        mPriority = SMALL_INTEGER_PRIORITY;
    }

    public static STSmallInteger create(int value) {
        STSmallInteger integer = new STSmallInteger(value);
        integer.setSTClass(Universe.classes().SmallInteger);
        return integer;
    }

    public int toInt() {
        return mData;
    }

    public STNumber asFloat() {
        return STFloating.create(mData);
    }

    public STNumber asLargeInteger() {
        return STLargeInteger.create(mData);
    }

    public STObject bitAnd(STSmallInteger second) {
        int result = mData & second.mData;
        return STSmallInteger.create(result);
    }

    public STObject bitShift(STSmallInteger second) {
        int result = mData << second.mData;
        return STSmallInteger.create(result);
    }

    public STObject bitXor(STSmallInteger second) {
        int result = mData ^ second.mData;
        return STSmallInteger.create(result);
    }

    public STObject bitOr(STSmallInteger second) {
        int result = mData | second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber castToPriority(int maxPriority) {
        if (maxPriority == LARGE_INTEGER_PRIORITY) {
            return asLargeInteger();
        } else if (maxPriority == FLOAT_INTEGER_PRIORITY) {
            return asFloat();
        }

        throw new RuntimeException("Unsopported large integer cast");
    }

    @Override
    protected STNumber _add(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        int result = mData + second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _substract(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        int result = mData - second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _multiply(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        int result = mData * second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _divide(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        int result = mData / second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _mod(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        int result = mData % second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STObject _lessThen(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData < second.mData) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _greaterThen(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData > second.mData) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _lessEqual(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData <= second.mData) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _greaterEqual(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData >= second.mData) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _equal(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData == second.mData) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _notEqual(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData != second.mData) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }
}
