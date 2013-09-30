package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STSmallInteger extends STNumber {

    private static final long serialVersionUID = 1L;

    private int mData;

    private STSmallInteger(int data) {
        mData = data;
        mPriority = SMALL_INTEGER_PRIORITY;
    }

    public static STSmallInteger create(int value) {
        STSmallInteger integer = new STSmallInteger(value);
        integer.setClassProvider(new BindingClassProvider(integer) {
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().SmallInteger;
            }
        });
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

        SignalSuite.error("STSmallInteger cast not supported %d", maxPriority);
        return null;
    }

    static final boolean isPreventAddOverfow(int left, int right) {
        if (right > 0 ? left > Integer.MAX_VALUE - right
                : left < Integer.MIN_VALUE - right) {
            return true;
        }
        
        return false;
    }

    static final boolean isPreventSubstractOverfow(int left, int right) {
        if (right > 0 ? left < Integer.MIN_VALUE + right
                : left > Integer.MAX_VALUE + right) {
            return true;
        }
        return false;
    }

    static final boolean isPreventMultiplyOverfow(int left, int right) {
        if (right > 0 ? left > Integer.MAX_VALUE / right
                || left < Integer.MIN_VALUE / right
                : (right < -1 ? left > Integer.MIN_VALUE / right
                        || left < Integer.MAX_VALUE / right : right == -1
                        && left == Integer.MIN_VALUE)) {
            return true;
        }
        return false;
    }
    
    static final boolean isPreventDivideOverfow(int left, int right) {
        if ((left == Integer.MIN_VALUE) && (right == -1)) {
            return true;
        }
        
        return false;
    }
    
    static final boolean isPreventAbsOverflow(int a) {
        if (a == Integer.MIN_VALUE) {
            return true;
        }
        
        return false;
    }
    
    static final boolean isPreventNegateOverflow(int a) {
        if (a == Integer.MIN_VALUE) {
            return true;
        }
        
        return false;
    }

    @Override
    protected STNumber _add(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if(isPreventAddOverfow(mData, second.mData)) {
            return add(this.asLargeInteger(), second.asLargeInteger());
        }
        
        int result = mData + second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _substract(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if(isPreventSubstractOverfow(mData, second.mData)) {
            return substract(this.asLargeInteger(), second.asLargeInteger());
        }
        
        int result = mData - second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _multiply(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if(isPreventMultiplyOverfow(mData, second.mData)) {
            return multiply(this.asLargeInteger(), second.asLargeInteger());
        }
        
        int result = mData * second.mData;
        return STSmallInteger.create(result);
    }

    @Override
    protected STNumber _divide(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        
        if(isPreventDivideOverfow(mData, second.mData)) {
            return divide(this.asLargeInteger(), second.asLargeInteger());
        }
        
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
            return ImageSuite.image().objects().TRUE;
        }

        return ImageSuite.image().objects().FALSE;
    }

    @Override
    protected STObject _greaterThen(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData > second.mData) {
            return ImageSuite.image().objects().TRUE;
        }

        return ImageSuite.image().objects().FALSE;
    }

    @Override
    protected STObject _lessEqual(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData <= second.mData) {
            return ImageSuite.image().objects().TRUE;
        }

        return ImageSuite.image().objects().FALSE;
    }

    @Override
    protected STObject _greaterEqual(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData >= second.mData) {
            return ImageSuite.image().objects().TRUE;
        }

        return ImageSuite.image().objects().FALSE;
    }

    @Override
    protected STObject _equal(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData == second.mData) {
            return ImageSuite.image().objects().TRUE;
        }

        return ImageSuite.image().objects().FALSE;
    }

    @Override
    protected STObject _notEqual(STNumber other) {
        STSmallInteger second = other.castToSubclass();
        if (mData != second.mData) {
            return ImageSuite.image().objects().TRUE;
        }

        return ImageSuite.image().objects().FALSE;
    }

    public String toString() {
        return Integer.toString(mData);
    }
}
