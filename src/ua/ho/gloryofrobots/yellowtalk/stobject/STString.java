package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STString extends STByteObject {

    private static final long serialVersionUID = 1L;

    public static STString create(String value) {
        STString str = new STString(value);
        str.setClassProvider(new BindingClassProvider(str) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().String;
            }
        });
        
        return str;
    }

    protected STString(String value) {
        super(value.getBytes());
    }

    public STSymbol toSymbol() {
        return STSymbol.unique(super.toString());
    }

    public STArray splitToSymbols(
            String splitter) {
        String data = toString();
        
        if(data.equals("")) {
            return null;
        }
        
        String [] strings = data.split(splitter);
        STArray arr = STArray.create(strings.length);
        int index = 0;
        for(String str : strings) {
            STSymbol symb = STSymbol.unique(str);
            arr.set(index, symb);
            index++;
        }
        
        return arr;
    }
}
