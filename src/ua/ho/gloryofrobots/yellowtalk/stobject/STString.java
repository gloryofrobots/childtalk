package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;

public class STString extends STByteObject {

    private static final long serialVersionUID = 1L;
    
    public static STString create(String value) {
        STString str = new STString(value);
        str.setSTClass(Universe.classes().String);
        return str;
        
    }
    
    protected STString(String value) {
        super(value.getBytes());
    }
    
    
}
