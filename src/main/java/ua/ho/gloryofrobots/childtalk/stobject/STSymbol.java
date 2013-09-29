package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STSymbol extends STString {
    private static final long serialVersionUID = 1L;
    
    public static STSymbol create(String str) {
        STSymbol symbol = ImageSuite.image().getSymbol(str);
        if(symbol == null) {
            symbol = new STSymbol(str);
            
            symbol.setClassProvider(new BindingClassProvider(symbol) {
                @Override
                protected STClass _getSTClass() {
                    return ImageSuite.image().classes().Symbol;
                }
            });
            
            ImageSuite.image().putSymbol(str, symbol);
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
