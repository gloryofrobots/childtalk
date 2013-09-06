package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.HashMap;

public class STSymbol extends STObject {
    private static final long serialVersionUID = 1L;
    private static HashMap<String, STSymbol> mStorage;
    
    public static STSymbol create(String str) {
        STSymbol symbol = mStorage.get(str);
        if(symbol == null) {
            symbol = new STSymbol(str);
            mStorage.put(str, symbol);
        }
        
        return symbol;
    }
    
    String mValue;
    private STSymbol(String val) {
        mValue = val;
    }
    
    
    public String toString() {
        return mValue;
    }
}
