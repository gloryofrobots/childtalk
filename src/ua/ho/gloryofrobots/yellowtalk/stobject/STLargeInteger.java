package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.math.BigInteger;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;

public class STLargeInteger extends STNumber {

    private static final long serialVersionUID = 1L;
    
    private BigInteger mData;
    
   
    private STLargeInteger(BigInteger data) {
        mData = data;
        mPriority = LARGE_INTEGER_PRIORITY;
    }

    public static STLargeInteger create(BigInteger data) {
        STLargeInteger largeInteger = new STLargeInteger(data);

        largeInteger.setSTClass(Universe.classes().LargeInteger);
        return largeInteger;
    }
    
    public static STObject create(long data) {
        BigInteger bigInt = BigInteger.valueOf(data);
        return create(bigInt);
    }
    
    public static STLargeInteger create(int data) {
        BigInteger bigInt = BigInteger.valueOf((long) data);
        return create(bigInt);
    }

    public STObject bitShift(int value) {
        BigInteger result = mData.shiftLeft(value);
        return STLargeInteger.create(result);
    }

    public STObject bitXor(int value) {
        BigInteger result = mData.xor(BigInteger.valueOf(value));
        return STLargeInteger.create(result);
    }

    public STObject bitOr(int value) {
        BigInteger result = mData.or(BigInteger.valueOf(value));
        return STLargeInteger.create(result);
    }

    public STObject bitAnd(int value) {
        BigInteger result = mData.and(BigInteger.valueOf(value));
        return STLargeInteger.create(result);
    }

    public STObject clear() {
        mData = BigInteger.valueOf(0);
        return this;
    }

    public STNumber asFloat() {
        byte[] bytes = mData.toByteArray();
        return STFloating.create(bytes);
    }

    @Override
    protected STNumber _add(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        BigInteger result = mData.add(second.mData);
        return STLargeInteger.create(result);
    }

    @Override
    protected STNumber _substract(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        BigInteger result = mData.subtract(second.mData);
        return STLargeInteger.create(result);
    }

    @Override
    protected STNumber _multiply(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        BigInteger result = mData.multiply(second.mData);
        return STLargeInteger.create(result);
    }

    @Override
    protected STNumber _divide(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        BigInteger result = mData.divide(second.mData);
        return STLargeInteger.create(result);
    }

    @Override
    protected STNumber _mod(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        BigInteger result = mData.mod(second.mData);
        return STLargeInteger.create(result);
    }

    @Override
    protected STObject _lessThen(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        int compared = mData.compareTo(second.mData);
        if (compared < 0) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _greaterThen(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        int compared = mData.compareTo(second.mData);
        if (compared > 0) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _lessEqual(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        int compared = mData.compareTo(second.mData);
        if (compared < 0 || compared == 0) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _greaterEqual(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        int compared = mData.compareTo(second.mData);
        if (compared > 0 || compared == 0) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _equal(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        int compared = mData.compareTo(second.mData);
        if (compared == 0) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _notEqual(STNumber other) {
        STLargeInteger second = other.castToSubclass();
        int compared = mData.compareTo(second.mData);
        if (compared != 0) {
            return Universe.objects().TRUE;
        }

        return Universe.objects().FALSE;
    }

    @Override
    protected STNumber castToPriority(int maxPriority) {
        if (maxPriority == FLOAT_INTEGER_PRIORITY) {
            return asFloat();
        }
        if (maxPriority == SMALL_INTEGER_PRIORITY) {
            return asSmallInteger();
        }
        
        SignalSuite.error("STLargeInteger cast not supported %d", maxPriority);
        return null;
    }
    
    private STNumber asSmallInteger() {
        return STSmallInteger.create(mData.intValue());
    }

    public String toString() {
        return mData.toString();
    }

    
}
