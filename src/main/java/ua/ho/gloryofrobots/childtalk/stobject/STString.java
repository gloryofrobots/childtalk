package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STString extends STByteObject {

    private static final long serialVersionUID = 1L;

    public static STString create(String value) {
        STString str = new STString(value);
        str.setClassProvider(new BindingClassProvider(str) {
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().String;
            }
        });
        
        return str;
    }

    protected STString(String value) {
        super(value.getBytes());
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
            STSymbol symb = STSymbol.create(str);
            arr.put(index, symb);
            index++;
        }
        
        return arr;
    }
}
