package ua.ho.gloryofrobots.childtalk.bootstrap;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import ua.ho.gloryofrobots.childtalk.ChildtalkApplicationInterface;
import ua.ho.gloryofrobots.childtalk.inout.InOutSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;

public class BootstrapSuite {
    private static ChildtalkApplicationInterface sApplication;
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

    public static void buildDefaultImage(String folder) {
        buildImage(folder, sDefaultClassFiles);
    }

    public static void buildImage(String folder, String[] classFiles) {
        ImageSuite.createNewImage();
        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(folder, classFiles,
                ImageSuite.image());
        ImageSuite.image().initialise();
    }

    public ChildtalkApplicationInterface application() {
        return sApplication;
    }

    public static boolean setApplication(
            ChildtalkApplicationInterface application) {
        sApplication = application;
        InOutSuite.init(application.getInputStream(),
                application.getOutputStream(), application.getErrorStream());
        return true;
    }

    public static boolean bootstrap() {
        PrimitivesSuite.initialisePrimitives();
        //DebugSuite.setDebugMode(DebugSuite.DEBUG_MODE_INTERPRETER);
        return true;
    }

    public static boolean loadImage(String path) {
        FileInputStream fis = null;
        ObjectInputStream oin = null;
        try {
             fis = new FileInputStream(path);
             oin = new ObjectInputStream(fis);
            STImage image = (STImage) oin.readObject();
            ImageSuite.setImage(image);
            ImageSuite.image().initialise();
        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        } finally {
            if(oin == null) {
                return false;
            }
            
            try {
                oin.close();
                return true;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        
        return false;
    }

    public static boolean saveCurrentImage(String imagePath) {
        FileOutputStream fos;
        ObjectOutputStream oos = null;
        try {
            fos = new FileOutputStream(imagePath);
            oos = new ObjectOutputStream(fos);
            STImage image = ImageSuite.image();
            oos.writeObject(image);
            
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(oos == null) {
                return false;
            }
            
            try {
                oos.flush();
                oos.close();
                return true;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        return false;
    }

    public static void loadAndEvalFiles(String[] filesToRun) {
        Loader loader = new Loader();
        STImage image = ImageSuite.image();
        for(String path : filesToRun) {
            loader.loadAndCompileFile(path, image);
        }
    }
}
