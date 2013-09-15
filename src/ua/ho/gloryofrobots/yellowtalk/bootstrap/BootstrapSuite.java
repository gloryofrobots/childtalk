package ua.ho.gloryofrobots.yellowtalk.bootstrap;

import java.io.InputStream;
import java.io.PrintStream;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.yellowtalk.inout.InOutSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;

public class BootstrapSuite {
    
    public static boolean bootstrap(InputStream stdin, PrintStream stdout, PrintStream stderr) {
        
        InOutSuite.init(stdin, stdout, stderr);
        Universe.bigbang();

        return true;
    }
    
    public static void loadDefaultSystem() {
        String folder = "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/core";
        String [] classNames = { "INITIALISE", "Object"," Behavior", "Metaclass"};
        
        Loader loader = new Loader();
        
        STImage image = Universe.image();
        loader.loadAndCompileClassesFromFolder(folder, classNames, image);
        
        
        
        Universe.beginToGrow();
        
        System.out.println(image.toString());
        /*
        sImage.put(STSymbol.unique("nil"), sObjects.NIL);
        sLoader.loadClassesFromFolder(folder, cl, sImage);
        
        fetchClasses(sImage);
        
        
        */
    }
    
    public static STObject evalFile(String path) {
        Loader loader = new Loader();
        ProgramTextStreamInterface programStream =  Loader.createProgramStream(path);
        STExecutableObject executable = CompileSuite.compileEval(programStream);
        STProcess process = SchedulingSuite.callExecutableInNewProcess(executable);
        STObject result =  process.getResult();     
        return result;
    }
    
    public static void main(String[] args) {
        BootstrapSuite.bootstrap(System.in, System.out, System.err);
        BootstrapSuite.loadDefaultSystem();
        BootstrapSuite.evalFile("/home/gloryofrobots/develop/smalltalk/yellowtalk/st/tests/compiler_test.st");
    }
    
}
