package ua.ho.gloryofrobots.childtalk.bootstrap;

import ua.ho.gloryofrobots.childtalk.stobject.STImage;

public class ImageSuite {
    private static STImage sImage;
    
    public static STImage image() {
        return sImage;
    }
    public static void setImage(STImage image) {
        sImage = image;
    }
    
    public static void createNewImage() {
        sImage = STImage.create();
        sImage.createBasic();
    }
}
