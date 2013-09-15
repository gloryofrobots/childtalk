package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STSymbol extends STObject {
    private static final long serialVersionUID = 1L;
    private static HashMap<String, STSymbol> mStorage = new HashMap<String, STSymbol>();
    
    public static STSymbol unique(String str) {
        STSymbol symbol = mStorage.get(str);
        if(symbol == null) {
            symbol = new STSymbol(str);
            
            symbol.setClassProvider(new BindingClassProvider(symbol) {
                @Override
                protected STClass _getSTClass() {
                    return Universe.classes().Symbol;
                }
            });
            
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
    
    public STString toSTString() {
        return STString.create(mValue);
    }
}
