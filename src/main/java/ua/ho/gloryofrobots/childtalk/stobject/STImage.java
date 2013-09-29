package ua.ho.gloryofrobots.childtalk.stobject;

import java.io.Serializable;
import java.util.HashMap;

public class STImage extends STScope {
    private static final long serialVersionUID = 1L;
    
    public  class Symbols implements Serializable{
        public STSymbol SELF;
        public STSymbol SUPER;
        public STSymbol ERROR_COLON;
        public STSymbol THIS_CONTEXT;
        public STSymbol PRINTNL;
        public STSymbol INITIALIZE;
    }

    public  class Objects implements Serializable{
        public STMetaclass METACLASS;
        public STObject TRUE;
        public STObject FALSE;
        public STObject NIL;
    }

    public  class Classes implements Serializable{
        public STClass InternalDictionary;
        public STClass System;
        public STClass Number;
        public STClass Transcript;
        public STClass Collection;
        public STClass Stack;
        public STClass Scope;
        public STClass Process;
        public STClass Method;
        public STClass Context;
        public STClass Array;
        public STClass Class;
        public STClass Smalltalk;
        public STClass Block;
        public STClass LargeInteger;
        public STClass SmallInteger;
        public STClass Float;
        public STClass Object;
        public STClass String;
        public STClass ByteArray;
        public STClass Symbol;
        public STClass Character;
        public STClass Dictionary;
        public STClass True;
        public STClass False;
        public STClass Metaclass;
        public STClass Behaviour;
        public STClass DateTime;
        public STClass Signal;
        public STClass UndefinedObject;
    }

    private Classes mClasses;
    private Objects mObjects;
    private Symbols mSymbols;
    private  HashMap<String, STSymbol> mSymbolStorage;
    
    public static STImage create() {
        STImage image =  new STImage();
        return image;
    }

    private STImage() {
        super();
        mSymbolStorage = new HashMap<String, STSymbol>();
        mObjects = new Objects();
        mSymbols = new Symbols();
        mClasses = new Classes();
    }
    
    public void createBasic() {
        mSymbols.SELF = STSymbol.create("self");
        mSymbols.SUPER = STSymbol.create("super");
        mSymbols.ERROR_COLON = STSymbol.create("error:");
        mSymbols.THIS_CONTEXT = STSymbol.create("thisContext");
        mSymbols.PRINTNL = STSymbol.create("printNl");
        mSymbols.INITIALIZE = STSymbol.create("initialize");
        mObjects.FALSE = STLabeledObject.create("false");
        
        mObjects.TRUE = STLabeledObject.create("true");
        mObjects.NIL = STClass.create("nil");
        mObjects.NIL.setSTClass(STProtoObject.get());
        put(STSymbol.create("nil"), mObjects.NIL);
    }
    
    public STSymbol getSymbol(String key) {
        return mSymbolStorage.get(key);
    }

    public void putSymbol(String key, STSymbol symbol) {
        mSymbolStorage.put(key, symbol);
    }
    
    public void initialise() {
        mClasses.Stack = getAndCast("Stack");
        mClasses.Scope = getAndCast("Scope");
        mClasses.Process = getAndCast("Process");
        mClasses.Method = getAndCast("Method");
        mClasses.Context = getAndCast("Context");
        mClasses.Array = getAndCast("Array");
        mClasses.Class = getAndCast("Class");
        mClasses.Smalltalk = getAndCast("Smalltalk");
        mClasses.Block = getAndCast("Block");
        mClasses.LargeInteger = getAndCast("LargeInteger");
        mClasses.SmallInteger = getAndCast("SmallInteger");
        mClasses.Float = getAndCast("Float");
        mClasses.Object = getAndCast("Object");
        mClasses.String = getAndCast("String");
        mClasses.ByteArray = getAndCast("ByteArray");
        mClasses.Symbol = getAndCast("Symbol");
        mClasses.Character = getAndCast("Character");
        mClasses.Dictionary = getAndCast("Dictionary");
        mClasses.True = getAndCast("True");
        mClasses.False = getAndCast("False");
        mClasses.Metaclass = getAndCast("Metaclass");
        mClasses.Behaviour = getAndCast("Behaviour");
        mClasses.DateTime = getAndCast("DateTime");
        mClasses.Signal = getAndCast("Signal");
        mClasses.Collection = getAndCast("Collection");
        mClasses.UndefinedObject = getAndCast("UndefinedObject");
        mClasses.Transcript = getAndCast("Transcript");
        mClasses.Number = getAndCast("Number");
        mClasses.InternalDictionary = getAndCast("InternalDictionary");
        mClasses.System = getAndCast("System");

        mObjects.NIL.setSTClass(mClasses.UndefinedObject);
        mObjects.FALSE.setSTClass(mClasses.False);
        mObjects.TRUE.setSTClass(mClasses.True);
        STProtoObject.get().setSuperClass(mClasses.Object);

        setSTClass(mClasses.System);
        put(STSymbol.create("SmallTalk"), this);
    }

    public Objects objects() {
        return mObjects;
    }

    public Classes classes() {
        return mClasses;
    }

    public Symbols symbols() {
        return mSymbols;
    }

    public <T extends STObject> T getAndCast(String key) {
        STSymbol symbol = STSymbol.create(key);
        return getAndCast(symbol);
    }

    @Override
    public String toString() {
        return "Image";
    }

   
}
