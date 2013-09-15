package ua.ho.gloryofrobots.yellowtalk;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Scheduler;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMetaclass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProtoObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

// God like object
public class Universe {
    private static STImage sImage;
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
        public  STClass Stack;
        public  STClass Scope;
        public  STClass Process;
        public  STClass Method;
        public  STClass Context;
        public  STClass Array;
        public  STClass Class;
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
        sImage = new STImage();
        sSymbols = new Symbols();
        sClasses = new Classes();
        
        sSymbols.SELF = STSymbol.unique("self");
        sSymbols.SUPER = STSymbol.unique("super");
        
        sObjects.FALSE = STObject.createEmpty();
        
        sObjects.TRUE = STObject.createEmpty();
        sObjects.NIL = STClass.create("nil");
        sObjects.NIL.setSTClass(STProtoObject.get());
    }
    
    public static STClass getClassFromImage(String name) {
        STSymbol symbol = STSymbol.unique(name);
        return  sImage.getAndCast(symbol);
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
        return sSignals;
    }


    public static void beginToGrow() {
        sClasses.Stack = sImage.getAndCast("Stack");
        sClasses.Scope = sImage.getAndCast("Scope");
        sClasses.Process = sImage.getAndCast("Process");
        sClasses.Method = sImage.getAndCast("Method");
        sClasses.Context = sImage.getAndCast("Context");
        sClasses.Array = sImage.getAndCast("Array");
        sClasses.Class = sImage.getAndCast("Class");
        sClasses.Smalltalk = sImage.getAndCast("Smalltalk");
        sClasses.Block = sImage.getAndCast("Block");
        sClasses.LargeInteger = sImage.getAndCast("LargeInteger");
        sClasses.SmallInteger = sImage.getAndCast("SmallInteger");
        sClasses.Float = sImage.getAndCast("Float");
        sClasses.Object = sImage.getAndCast("Object");
        sClasses.String = sImage.getAndCast("String");
        sClasses.ByteArray = sImage.getAndCast("ByteArray");
        sClasses.Symbol = sImage.getAndCast("Symbol");
        sClasses.Character = sImage.getAndCast("Character");
        sClasses.Dictionary = sImage.getAndCast("Dictionary");
        sClasses.True = sImage.getAndCast("True");
        sClasses.False = sImage.getAndCast("False");
        sClasses.Metaclass = sImage.getAndCast("Metaclass");
        sClasses.Behavior = sImage.getAndCast("Behavior");
        sClasses.DateTime = sImage.getAndCast("DateTime");
        
        sObjects.FALSE.setSTClass(sClasses.False);
        sObjects.TRUE.setSTClass(sClasses.True);
    }
}
