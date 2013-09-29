package ua.ho.gloryofrobots.childtalk.compilation;

import java.math.BigInteger;

public class Token {

    enum Type {
        UNKNOWN, END, INTEGER_CONST, LARGE_INTEGER_CONST, CHAR_CONST, FLOAT_CONST, CLOSING, ARRAY_BEGIN,

        STRING_ENTRY, NAME_CONST, NAME_COLON, SYMBOL_CONST, STRING_CONST, BINARY
    }
    
    public Token() {
        clear();
    }
    
    public String toString() {
        return String.format("<Token %s '%s'>", type.toString(), stringValue());
    }
    
    int mIntValue;
    BigInteger mLargeIntValue;
    double mFloatValue;
    private String mData;
    public Type type;

    private Token mNext = null;
    private Token mPrevious = null;
    int mPosition;

    public void evalSpecial(String data, Type type) {
        mData = data;
        this.type = type;
    }

    public void evalCharacter(char ch, Type type) {
        mData = new String();
        mData += ch;
        this.type = type;
    }

    public void evalString(String data) {
        mData = data;
        this.type = Type.STRING_CONST;
    }

    public boolean evalInteger(String data, int radix) {
        try {
            mIntValue = Integer.parseInt(data, radix);
            this.type = Type.INTEGER_CONST;
            
            mData = Integer.toString(mIntValue);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public boolean evalLargeInteger(String data, int radix) {
        try {
            mLargeIntValue = new BigInteger(data, radix);
            mData = mLargeIntValue.toString();
            this.type = Type.LARGE_INTEGER_CONST;
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public boolean evalFloat(String data) {
        try {
            mFloatValue = Double.parseDouble(data);
            mData = Double.toString(mFloatValue);
            this.type = Type.FLOAT_CONST;
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public boolean equalValue(String val) {
        return mData.equals(val);
    }

    public boolean equalValue(char val) {
        return charValue() == val;
    }

    public int integerValue() {
        return mIntValue;
    }

    public double floatValue() {
        return mFloatValue;
    }

    public char charValue() {
        return mData.charAt(0);
    }

    public BigInteger largeIntegerValue() {
        return mLargeIntValue;
    }

    public String stringValue() {
        return mData;
    }

    public int getPosition() {
        return mPosition;
    }

    public void setPosition(int position) {
        mPosition = position;
    }

    public Token getNext() {
        return mNext;
    }

    public void setNext(Token next) {
        this.mNext = next;
    }

    public Token getPrevious() {
        return mPrevious;
    }

    public void setPrevious(Token previous) {
        this.mPrevious = previous;
    }

    public Token getLast() {
        Token token = this;
        while (token.mNext != null) {
            token = token.mNext;
        }

        return token;
    }

    public void clear() {
        mIntValue = 0;
        mLargeIntValue = null;
        mFloatValue = 0;
        mData = null;
        type = Type.UNKNOWN;
    }
};
