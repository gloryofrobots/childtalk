package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.HashMap;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STSymbol extends STString {
    private static final long serialVersionUID = 1L;
    private static HashMap<String, STSymbol> mStorage = new HashMap<String, STSymbol>();
    
    public static STSymbol create(String str) {
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
    
    private STSymbol(String val) {
        super(val);
    }
    
    public STString toSTString() {
        return this;
    }
    
    public STSymbol toSymbol() {
        return this;
    }
}
