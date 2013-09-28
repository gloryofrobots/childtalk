package ua.ho.gloryofrobots.childtalk.bootstrap;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;

public class ImageBuilder {
    private  String[] mDefaultClassFiles = { "Object.st", "Behaviour.st",
            "UndefinedObject.st", "Boolean.st", "Metaclass.st",
            "Transcript.st", "Collection.st", "Set.st",
            "SequenceableCollection.st", "Interval.st", "ArrayedCollection.st",
            "Array.st", "ByteArray.st", "OrderedCollection.st", "Stack.st",
            "Bag.st", "Context.st", "Block.st", "Method.st", "Process.st",
            "Stream.st", "PositionableStream.st", "Random.st", "ReadStream.st",
            "WriteStream.st", "ReadWriteStream.st", "ByteStream.st",
            "FileStream.st", "StdIOStream.st", "String.st", "Symbol.st",
            "Magnitude.st", "Character.st", "Number.st", "Date.st",
            "DateTime.st", "Duration.st", "Integer.st", "SmallInteger.st",
            "LargeInteger.st", "Fraction.st", "Float.st", "Boolean.st",
            "True.st", "False.st", "Association.st", "VariableBinding.st",
            "Dictionary.st", "InternalDictionary.st", "System.st", "Signal.st",
            "Exception.st" };
    
    public  void buildDefaultSystem(String folder) {
        BootstrapSuite.buildImage(folder, mDefaultClassFiles);
    }
}
