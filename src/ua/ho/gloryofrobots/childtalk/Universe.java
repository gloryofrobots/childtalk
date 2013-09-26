package ua.ho.gloryofrobots.childtalk;

import ua.ho.gloryofrobots.childtalk.scheduler.Scheduler;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;
import ua.ho.gloryofrobots.childtalk.stobject.STLabeledObject;
import ua.ho.gloryofrobots.childtalk.stobject.STMetaclass;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProtoObject;
import ua.ho.gloryofrobots.childtalk.stobject.STSignal;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

// God like object
public class Universe {
    private static STImage sImage;
    private static String[] sCoreClasses = {
        //TODO remove to class definition
        "INITIALISE",
        "Object",
        "Behaviour",
        "UndefinedObject",
        "Class",
        "Metaclass",
        "Collection",
        "Set",
        "SequenceableCollection",
        "Interval",
        "ArrayedCollection",
        "Array",
        "ByteArray",
        "OrderedCollection",
        "Stack",
        "Bag",
        "Block",
        "Stream",
        "PositionableStream",
        "Random",
        "ReadStream",
        "WriteStream",
        "ReadWriteStream",
        "ByteStream",
        "FileStream",
        "StdIOStream",
        "String",
        "Symbol",
        "Magnitude",
        "Character",
        "Number",
        "Date",
        "DateTime",
        "Duration",
        "Integer",
        "SmallInteger",
        "LargeInteger",
        "Fraction",
        "Float",
        "Boolean",
        "True",
        "False",
        "Association",
        "VariableBinding",
        "Dictionary",
        "IdentityDictionary",
        "SystemDictionary",
        "Signal",
        "Exception",
        "Object",
        "Behaviour",
        "UndefinedObject",
        "Metaclass",
        "Set",
        "SequenceableCollection",
        "Interval",
        "ArrayedCollection",
        "Array",
        "ByteArray",
        "OrderedCollection",
        "Stack",
        "Bag",
        "Block",
        "PositionableStream",
        "Random",
        "WriteStream",
        "FileStream",
        "String",
        "Symbol",
        "Magnitude",
        "Character",
        "Number",
        "Date",
        "DateTime",
        "Integer",
        "SmallInteger",
        "LargeInteger",
        "Fraction",
        "Float",
        "True",
        "False",
        "Association",
        "VariableBinding",
        "Dictionary",
        "IdentityDictionary",
        "SystemDictionary",
        "Signal",
        "Error",
        "NotImplemented",
        "ZeroDivide",
        "PrimitiveFailed",
        "WrongArgumentCount",
        "MessageNotUnderstood",
        "NotYetImplemented",
        "ShouldNotImplement",
        "SubclassResponsibility",
        "InvalidValue",
        "InvalidArgument",
        "NotFound",
        "NotKeyed",
        "Exception",
        "MessageNotUnderstood",
        "SubclassResponsibility",
        "WrongArgumentCount",
        "NotFound"
        };
    
    //TODO String isKindOf Object. Object must setted as subclass to protoobject.
    
    public static class Symbols{
        public STSymbol SELF;
        public STSymbol SUPER;
        public STSymbol ERROR_COLON;
        public STSymbol THIS_CONTEXT;
        public STSymbol PRINT;
    }
    
    
    public static class Objects{
        public  STMetaclass METACLASS = null;
        public  STObject TRUE;
        public  STObject FALSE;
        public  STObject NIL;
    }
    /*
    public static class Signals{
        public  STObject PrimitiveError = STSignal.create();
        public  STObject RuntimeError = STSignal.create();
        public  STObject TypeError = STSignal.create();
    }
    */
    public static class Classes{
        public  STClass InternalDictionary;
        public  STClass System;
        public  STClass Number;
        public  STClass Transcript;
        public  STClass Collection;
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
        public  STClass Behaviour;
        public  STClass DateTime;
        public  STClass Signal;
        public  STClass UndefinedObject;
    }
    
    private static Classes sClasses;
//    private static Signals sSignals;
    
    private static Objects sObjects;
    private static Symbols sSymbols;
    
    private Universe(){}
    
    public static void bigbang(){
        sObjects = new Objects();
        sImage = new STImage();
        sSymbols = new Symbols();
        sClasses = new Classes();
        
        sSymbols.SELF = STSymbol.create("self");
        sSymbols.SUPER = STSymbol.create("super");
        sSymbols.ERROR_COLON = STSymbol.create("error:");
        sSymbols.THIS_CONTEXT = STSymbol.create("thisContext");
        sSymbols.PRINT = STSymbol.create("print");
        
        sObjects.FALSE = STLabeledObject.create("false");
        
        sObjects.TRUE = STLabeledObject.create("true");
        sObjects.NIL = STClass.create("nil");
        sObjects.NIL.setSTClass(STProtoObject.get());
    }
    
    public static STClass getClassFromImage(String name) {
        STSymbol symbol = STSymbol.create(name);
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

//    public static Signals signals() {
//        return sSignals;
//    }


    public static void beginToGrow() {
        //All basic creatures
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
        sClasses.Behaviour = sImage.getAndCast("Behaviour");
        sClasses.DateTime = sImage.getAndCast("DateTime");
        sClasses.Signal = sImage.getAndCast("Signal");
        sClasses.Collection = sImage.getAndCast("Collection");
        sClasses.UndefinedObject = sImage.getAndCast("UndefinedObject");
        sClasses.Transcript = sImage.getAndCast("Transcript");
        sClasses.Number = sImage.getAndCast("Number");
        sClasses.InternalDictionary = sImage.getAndCast("InternalDictionary");
        sClasses.System = sImage.getAndCast("System");
        
        //Quarks an gravitons
        sObjects.NIL.setSTClass(sClasses.UndefinedObject);
        sObjects.FALSE.setSTClass(sClasses.False);
        sObjects.TRUE.setSTClass(sClasses.True);
        STProtoObject.get().setSuperClass(sClasses.Object);
        
        //And God
        sImage.setSTClass(sClasses.System);
        sImage.put(STSymbol.create("SmallTalk"), sImage);
    }
}
