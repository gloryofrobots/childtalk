package ua.ho.gloryofrobots.yellowtalk;

import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class Universe {
    private static STImage mImage;
    public static class Objects{
        public  STObject TRUE;
        public  STObject FALSE;
        public  STObject NIL;
    }
    
    public static Objects objects;
    
    
    private Universe(){
    }
    
    public static void bigbang(){
        
    }
    
    public static STClass getClassFromImage(String name) {
        STSymbol symbol = STSymbol.create(name);
        return  mImage.getAndCast(symbol);
    }
    
   
    public static STImage getImage() {
        return mImage;
    }
}
