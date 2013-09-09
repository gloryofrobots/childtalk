package ua.ho.gloryofrobots.yellowtalk;

import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMetaclass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class Universe {
    private static STImage sImage;
    private static Loader sLoader;
    private static String[] sCoreClasses = {
        "Object",
        "Behaviour",
        "Class",
        "ByteArray",
        "String"
    };
    
  
    public static class Symbols{
        STSymbol SELF;
        STSymbol SUPER;
    }
    
    
    public static class Objects{
        public  STMetaclass METACLASS = null;
        public  STObject TRUE;
        public  STObject FALSE;
        public  STObject NIL;
    }
    
    public static class Signals{
        
        public  STObject PrimitiveError;
        public  STObject TypeError;
       
    }
    
    public static class Classes{
        public  STClass Smalltalk;
        public  STClass Block;
        public  STClass LargeInteger;
        public  STClass SmallInteger;
        public  STClass Float;
        public  STClass Object;
        public  STClass String;
        public  STClass ByteArray;
        public  STClass Symbol;
        public  STClass Character;
        public  STClass Dictionary;
        public  STClass True;
        public  STClass False;
        public  STClass Metaclass;
        public  STClass Behavior;
        public  STClass DateTime;
    }
    
    private static Classes sClasses;
    private static Signals sSignals;
    
    private static Objects sObjects;
    private static Symbols sSymbols;
    
    private Universe(){}
    
    
    public static void bigbang(){
        sObjects = new Objects();
        sLoader = new Loader();
        sImage = new STImage();
        sSymbols = new Symbols();
        sClasses = new Classes();
        
        sSymbols.SELF = STSymbol.unique("self");
        sSymbols.SUPER = STSymbol.unique("super");
        
        sObjects.FALSE = new STObject();
        sObjects.TRUE = new STObject();
        sObjects.NIL = STClass.create("nil");
        /*
        String folder = "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/core";
        String [] cl = {"bootstrap.st"};
        
        sImage.put(STSymbol.unique("nil"), sObjects.NIL);
        sLoader.loadClassesFromFolder(folder, cl, sImage);
        
        fetchClasses(sImage);
        
        sObjects.FALSE.setSTClass(sClasses.False);
        sObjects.TRUE.setSTClass(sClasses.True);
        */
    }
    
    private static void fetchClasses(STImage image) {
        sClasses.ByteArray = image.getAndCast("ByteArray");
        sClasses.False = image.getAndCast("False");
        sClasses.True = image.getAndCast("True");
        
    }

    public static STClass getClassFromImage(String name) {
        STSymbol symbol = STSymbol.unique(name);
        return  sImage.getAndCast(symbol);
    }
    
    public static Loader loader() {
        return sLoader;
    }
    
    public static STImage image() {
        return sImage;
    }
    
    public static Objects objects() {
        return sObjects;
    }
    
    public static Classes classes() {
        return sClasses;
    }
   
    public static Symbols symbols() {
        return sSymbols;
    }

    public static Signals signals() {
        // TODO Auto-generated method stub
        return sSignals;
    }
}
