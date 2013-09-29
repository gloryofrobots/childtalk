package ua.ho.gloryofrobots.childtalk.consoleapp;

import ua.ho.gloryofrobots.childtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;

public class ImageBuilder {
    private static String[] sDefaultClassFiles = { "Object.st", "Behaviour.st", "Metaclass.st",
        "UndefinedObject.st", "Boolean.st", 
        "Transcript.st", "Collection.st", "Set.st",
        "SequenceableCollection.st", "Interval.st", "ArrayedCollection.st",
        "Array.st", "ByteArray.st", "OrderedCollection.st", "Stack.st",
        "Bag.st", "Context.st", "Block.st", "Process.st", "Stream.st",
        "PositionableStream.st", "Random.st", "String.st", "Symbol.st",
        "Magnitude.st", "Character.st", "Number.st", "Date.st",
        "DateTime.st", "Integer.st", "SmallInteger.st", "LargeInteger.st",
        "Fraction.st", "Float.st", "Boolean.st", "True.st", "False.st",
        "Association.st", "VariableBinding.st", "Dictionary.st",
        "InternalDictionary.st", "System.st", "Signal.st", "Exception.st" };
    
    public  STImage buildDefaultImage(String folder) {
       STImage image = buildImage(folder, sDefaultClassFiles);
       return image;
    }

    private void saveImage(STImage image, String imagePath) {
        if(ImageSuite.saveImage(image, imagePath) == false) {
            System.err.println("Image writing error " + imagePath);
        }
    }

    public  STImage buildImage(String folder, String[] classFiles) {
        STImage image = ImageSuite.createImage();
        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(folder, classFiles,
                image);
        image.initialise();
        return image;
    }

    public static void main(String[] args) {
        String path = System.getenv("CHILDTALK_PATH");
        String folder = "/"+path+"/st/core";
        String imagePath = "/"+path+"/image/default.sim";
        
        ///home/gloryofrobots/develop/smalltalk/childtalk/st/core
        BootstrapSuite.setApplication(new Platform());
        ImageBuilder builder = new ImageBuilder();
        STImage image = builder.buildDefaultImage(folder);
        builder.saveImage(image, imagePath);
    }
}
