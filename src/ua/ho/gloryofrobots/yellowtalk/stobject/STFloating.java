package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.math.BigInteger;
import java.nio.ByteBuffer;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;

public class STFloating extends STNumber {

    private static final long serialVersionUID = 1L;

    private double mData;

    private STFloating(double data) {
        mData = data;
        mPriority = FLOAT_INTEGER_PRIORITY;
    }
        
    public static STFloating create(double data) {
        STFloating floating = new STFloating(data);
        floating.setSTClass(Universe.classes().Float);
        return floating;
    }
    
    public static STFloating create(byte[] bytes) {
        double data = ByteBuffer.wrap(bytes).getDouble();
        return create((double)data);
    }
    
    public static STFloating create(int data) {
        return create((double)data);
    }

    public STObject floor() {
        double result = Math.floor(mData);
        return STFloating.create(result);
    }

    public STObject truncate() {
        Double temp = new Double(mData);
        
        return STFloating.create(temp.longValue());
    }

    public STObject ceil() {
        double result = Math.ceil(mData);
        return STFloating.create(result);
    }

    @Override
    protected STNumber castToPriority(int maxPriority) {
        SignalSuite.error("STFloat cast not supported");
        return null;
    }

    @Override
    protected STNumber _add(STNumber other) {
        STFloating second = other.castToSubclass();
        double result = mData + second.mData;
        return STFloating.create(result);
    }

    @Override
    protected STNumber _substract(STNumber other) {
        STFloating second = other.castToSubclass();
        double result = mData - second.mData;
        return STFloating.create(result);
    }

    @Override
    protected STNumber _multiply(STNumber other) {
        STFloating second = other.castToSubclass();
        double result = mData * second.mData;
        return STFloating.create(result);
    }

    @Override
    protected STNumber _divide(STNumber other) {
        STFloating second = other.castToSubclass();
        double result = mData / second.mData;
        return STFloating.create(result);
    }

    @Override
    protected STNumber _mod(STNumber other) {
        STFloating second = other.castToSubclass();
        double result = mData % second.mData;
        return STFloating.create(result);
    }

    @Override
    protected STObject _lessThen(STNumber other) {
        STFloating second = other.castToSubclass();
        if (mData < second.mData) {
            return Universe.objects().TRUE;
        }
        
        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _greaterThen(STNumber other) {
        STFloating second = other.castToSubclass();
        if (mData > second.mData) {
            return Universe.objects().TRUE;
        }
        
        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _lessEqual(STNumber other) {
        STFloating second = other.castToSubclass();
        if (mData <= second.mData) {
            return Universe.objects().TRUE;
        }
        
        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _greaterEqual(STNumber other) {
        STFloating second = other.castToSubclass();
        if (mData >= second.mData) {
            return Universe.objects().TRUE;
        }
        
        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _equal(STNumber other) {
        STFloating second = other.castToSubclass();
        if (mData == second.mData) {
            return Universe.objects().TRUE;
        }
        
        return Universe.objects().FALSE;
    }

    @Override
    protected STObject _notEqual(STNumber other) {
        STFloating second = other.castToSubclass();
        if (mData != second.mData) {
            return Universe.objects().TRUE;
        }
        
        return Universe.objects().FALSE;
    }
}
