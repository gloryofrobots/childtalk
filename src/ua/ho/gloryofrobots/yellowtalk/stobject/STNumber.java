package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;

public abstract class STNumber extends STObject {
    private static final long serialVersionUID = 1L;

    public static final int SMALL_INTEGER_PRIORITY = 0;
    public static final int LARGE_INTEGER_PRIORITY = 1;
    public static final int FLOAT_INTEGER_PRIORITY = 2;

    protected  int mPriority;

    public static int getMaxPriority(STNumber first, STNumber second) {
        int firstPriority = first.getPriority();
        int secondPriority = second.getPriority();
        return Math.max(firstPriority, secondPriority);
    }

    public static STNumber add(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._add(castedSecond);
    }

    public static STNumber substract(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._substract(castedSecond);
    }

    public static STNumber multiply(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._multiply(castedSecond);
    }

    public static STNumber divide(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._divide(castedSecond);
    }

    public static STNumber mod(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._mod(castedSecond);
    }

    public static STObject lessThen(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._lessThen(castedSecond);
    }

    public static STObject greaterThen(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._greaterThen(castedSecond);
    }

    public static STObject lessEqual(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._lessEqual(castedSecond);
    }

    public static STObject greaterEqual(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._greaterEqual(castedSecond);
    }

    public static STObject equal(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._equal(castedSecond);
    }

    public static STObject notEqual(STNumber first, STNumber second) {
        int maxPriority = getMaxPriority(first, second);
        STNumber castedFirst = first.convert(maxPriority);
        STNumber castedSecond = second.convert(maxPriority);
        return castedFirst._notEqual(castedSecond);
    }

    public STNumber convert(int priority) {
        if (getPriority() == priority) {
            return this;
        }
        
        return castToPriority(priority);
    }

    protected abstract STNumber castToPriority(int priority);

    public int getPriority() {
        return mPriority;
    }

    protected abstract STNumber _add(STNumber other);

    protected abstract STNumber _substract(STNumber other);

    protected abstract STNumber _multiply(STNumber other);

    protected abstract STNumber _divide(STNumber other);

    protected abstract STNumber _mod(STNumber other);

    protected abstract STObject _lessThen(STNumber other);

    protected abstract STObject _greaterThen(STNumber other);

    protected abstract STObject _lessEqual(STNumber other);

    protected abstract STObject _greaterEqual(STNumber other);

    protected abstract STObject _equal(STNumber other);

    protected abstract STObject _notEqual(STNumber other);
    
    public static void main(String[] args) {
        Universe.bigbang();
        int[] check = new int[20];
        check[0] = STFloating.create(2.2).getPriority();
        check[1] = STSmallInteger.create(2).getPriority();
        check[2] = STLargeInteger.create(2).getPriority();
        check[3] = STLargeInteger.create(2).getPriority();
        check[4] = STSmallInteger.create(2).getPriority();
        check[5] = STFloating.create(2.2).getPriority();
        for(int val: check) {
            System.out.println(val);
        }
    }
}
