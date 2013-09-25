package ua.ho.gloryofrobots.yellowtalk.tests;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.yellowtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.yellowtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.yellowtalk.scheduler.EvalSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class SmallTalkTest {
    private void testPrimitives() {
        List<STSymbol> primitives = new ArrayList<STSymbol>();
        List<STObject> objects = Universe.image().asList();
        for (STObject obj : objects) {
            if(obj instanceof STClass == false) {
                continue;
            }
            STClass klass = (STClass) obj;
            klass.getUnknownPrimitives(primitives);
        }
        
        System.out.println(primitives);
    }

    private void runSmalltalkTestSuite() {
        String folder = "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/test";
        String[] classNames = { "TestCase", "ObjectTest", "FalseTest",
                "UndefinedObjectTest", "TrueTest", "CommonTest", "TestSuite" };

        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(folder, classNames,
                Universe.image());
        
        System.out.println("----------------------------------------------");
       
        STObject result = EvalSuite.evalFile("/home/gloryofrobots/develop/smalltalk/yellowtalk/st/test/run.st");
        System.out.println("    Result: " + result.toString());
    }

    private void runTests() {
        initSystem();
        testPrimitives();
        runSmalltalkTestSuite();
    }

    private void initSystem() {
        BootstrapSuite.bootstrap(System.in, System.out, System.err);
       
        BootstrapSuite.loadDefaultSystem();
    }

    public static void main(String[] args) {
        SmallTalkTest test = new SmallTalkTest();
        test.runTests();
    }
}


//puts ("- Test class variables");
//lexer = syx_lexer_new
//("Object subclass: #TestClass instanceVariableNames: '' classVariableNames: 'TestVar'!"
//"!TestClass class methodsFor: 'testing'!"
//"initialize TestVar := 123! testVar ^TestVar ! !"
//"!TestClass methodsFor: 'testing'!"
//"testVar ^TestVar ! !");
//ok = syx_cold_parse (lexer);
//assert (ok == TRUE);
//syx_lexer_free (lexer, FALSE);
//
//ret_obj = _interpret
//("method TestClass initialize. ^TestClass testVar + TestClass new testVar");
//assert (SYX_SMALL_INTEGER(ret_obj) == 123 + 123);


