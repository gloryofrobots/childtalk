package ua.ho.gloryofrobots.childtalk.consoleapp;

import ua.ho.gloryofrobots.childtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.childtalk.scheduler.EvalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class SmallTalkTest {
    private void runSmalltalkTestSuite(String testsFolder) {
        
        String[] classNames = { "TestCase.st", "ObjectTest.st", "FalseTest.st",
                "UndefinedObjectTest.st", "TrueTest.st", "CommonTest.st", "TestSuite.st" };

        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(testsFolder, classNames,
                ImageSuite.image());
        
        System.out.println("----------------------------------------------");
       
        STObject result = EvalSuite.evalFile(testsFolder + "run.st");
        System.out.println("    Result: " + result.toString());
    }
    
    public void error(String format, Object... args) {
        String txt = String.format(format, args);
        System.err.println(txt);
        System.exit(-1);
    }
    
    public void _run(String testsFolder) {
        testPrimitives();
        runSmalltalkTestSuite(testsFolder);
    }
    
    private void runAfterLoadingImage(String testsFolder , String imagePath) {
        System.out.println("LOADING EXISTED IMAGE");
        //first load image
        if(ImageSuite.loadImage(imagePath) == false) {
            error("Error loading image %s", imagePath);
        }
        System.out.println("RUN TESTS");
        _run(testsFolder);
    }
    private void runAfterBuildingImage(String testsFolder , String coreFolder) {
        System.out.println("BUILDING IMAGE");
        //create image from core
        ImageSuite.clear();
        ImageBuilder builder = new ImageBuilder();
        builder.buildDefaultImage(coreFolder);
        System.out.println("RUN TESTS AGAIN");
        _run(testsFolder);
    }
    
    public void run(String testsFolder, String coreFolder, String imagePath) {
        BootstrapSuite.setApplication(new Platform());
        runAfterLoadingImage(testsFolder, imagePath);
        runAfterBuildingImage(testsFolder, coreFolder);
    }

    private void testPrimitives() {
        new PrimitiveTester().test();
    }

    public static void main(String[] args) {
        String path = System.getenv("CHILDTALK_PATH");
        String testsFolder = path+"/st/tests/";
        String coreFolder = path+"/st/core/";
        String imagePath = path+"/src/main/resources/image/default.sim";
        
        SmallTalkTest test = new SmallTalkTest();
        test.run(testsFolder, coreFolder, imagePath);
    }
}       