package ua.ho.gloryofrobots.yellowtalk.bootstrap;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.yellowtalk.inout.InOutSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class BootstrapSuite {

    public static boolean bootstrap(InputStream stdin, PrintStream stdout,
            PrintStream stderr) {
        
        InOutSuite.init(stdin, stdout, stderr);
        Universe.bigbang();
        //DebugSuite.setDebugMode(DebugSuite.DEBUG_MODE_COMPILER, DebugSuite.DEBUG_MODE_INTERPRETER);
        return true;
    }

    public static void loadDefaultSystem() {
        String folder = "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/core";
        String[] classNames = { "INITIALISE" };

        Loader loader = new Loader();

        STImage image = Universe.image();
        loader.loadAndCompileClassesFromFolder(folder, classNames, image);
        Universe.beginToGrow();
        PrimitivesSuite.initialisePrimitives();
        List<STObject> objects = image.asList();
        for (STObject obj : objects) {
            STClass klass = (STClass) obj;
            STSymbol name = klass.getName();
            loader.loadAndCompileClass(folder, name.toString(), image);
        }
        
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_COMPILER, image.toString());
    }

    public static STObject evalFile(String path) {
        Loader loader = new Loader();
        ProgramTextStreamInterface programStream = Loader
                .createProgramStream(path);
        STExecutableObject executable = CompileSuite.compileEval(programStream);
        STProcess process = SchedulingSuite
                .callExecutableInNewProcess(executable);
        STObject result = process.getResult();
        System.out.println("    Result: " + result.toString());
        return result;
    }
    
    private static void runTests() {
        BootstrapSuite.testPrimitives();
        BootstrapSuite.runSmalltalkTestSuite();
       
    }
    
    private static void runSmalltalkTestSuite() {
        String folder = "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/test";
        String[] classNames = { "TestCase","ObjectTest","FalseTest","TrueTest","SimpleTest","TestSuite" };
        
        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(folder, classNames, Universe.image());
        System.out.println("----------------------------------------------");
        /*BootstrapSuite
                .evalFile("/home/gloryofrobots/develop/smalltalk/yellowtalk/st/tests/compiler_test.st");*/
        BootstrapSuite
        .evalFile("/home/gloryofrobots/develop/smalltalk/yellowtalk/st/test/run.st");
    }

    public static void main(String[] args) {
        BootstrapSuite.bootstrap(System.in, System.out, System.err);
        BootstrapSuite.loadDefaultSystem();
        BootstrapSuite.runTests();
    }

    private static void testPrimitives() {
        // TODO Auto-generated method stub
        List<STObject> objects = Universe.image().asList();
        for (STObject obj : objects) {
            STClass klass = (STClass) obj;
            List<STSymbol> prims = klass.getUnknownPrimitives();
            if (prims.isEmpty()) {
                continue;
            }

            System.out.println(prims);
        }
    }

}
