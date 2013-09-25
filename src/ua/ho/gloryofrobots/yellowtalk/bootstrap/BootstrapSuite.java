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
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class BootstrapSuite {

    public static boolean bootstrap(InputStream stdin, PrintStream stdout,
            PrintStream stderr) {

        InOutSuite.init(stdin, stdout, stderr);
        Universe.bigbang();
        //DebugSuite.setDebugMode(DebugSuite.DEBUG_MODE_COMPILER,
        //        DebugSuite.DEBUG_MODE_INTERPRETER);
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
            if(obj instanceof STClass == false) {
                continue;
            }
            
            STClass klass = (STClass) obj;
            STSymbol name = klass.getName();
            loader.loadAndCompileClass(folder, name.toString(), image);
            
        }

        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_COMPILER, image.toString());
    }

    
}

