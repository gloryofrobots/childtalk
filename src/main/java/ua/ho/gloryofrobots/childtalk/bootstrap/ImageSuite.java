package ua.ho.gloryofrobots.childtalk.bootstrap;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class ImageSuite {
    private static STImage sImage;

    public static STImage image() {
        return sImage;
    }

    public static void setImage(STImage image) {
        sImage = image;
    }

    public static STImage createImage() {
        sImage = STImage.create();
        sImage.createBasic(); 
        return sImage;
    }
    
    public static void clear() {
        sImage = null;
    }
 
    public static boolean loadImage(String path) {
        FileInputStream fis = null;
        try {
            fis = BootstrapSuite.application().openFileInputStream(path);
            return loadImageFromStream(fis);
        } catch (IOException e) {
            e.printStackTrace();
        }

        return false;
    }

    public static boolean loadImageFromResource(String path) {
        InputStream stream = ImageSuite.class.getResourceAsStream(path);
        if (stream == null) {
            return false;
        }

        return loadImageFromStream(stream);
    }

    private static boolean loadImageFromStream(InputStream stream) {
        ObjectInputStream oin = null;
        try {
            oin = new ObjectInputStream(stream);
            STImage image = (STImage) oin.readObject();
            setImage(image);
            image.initialise();

            return true;
        } catch (ClassNotFoundException | IOException e) {
            e.printStackTrace();
        } finally {
            if (oin == null) {
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
        return saveImage(image(), imagePath);
    }
    
    public static boolean saveImage(STImage image, String imagePath) {
        FileOutputStream fos;
        ObjectOutputStream oos = null;
        try {
            fos = BootstrapSuite.application().openFileOutputStream(imagePath);
            oos = new ObjectOutputStream(fos);
            STClass nilClass = (STClass) image.objects().NIL;
            //nilClass.clear();
            //oos.writeObject(nilClass.getScope());
            oos.writeObject(nilClass);

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (oos == null) {
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
}
