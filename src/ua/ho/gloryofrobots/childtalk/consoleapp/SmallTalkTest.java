package ua.ho.gloryofrobots.childtalk.consoleapp;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.childtalk.ChildtalkApplicationInterface;
import ua.ho.gloryofrobots.childtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.childtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.childtalk.scheduler.EvalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STMethod;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;
import ua.ho.gloryofrobots.childtalk.stobject.STScope;
import ua.ho.gloryofrobots.childtalk.stobject.STString;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class SmallTalkTest {
    private void runSmalltalkTestSuite() {
        String folder = "/home/gloryofrobots/develop/smalltalk/childtalk/st/tests/";
        String[] classNames = { "TestCase.st", "ObjectTest.st", "FalseTest.st",
                "UndefinedObjectTest.st", "TrueTest.st", "CommonTest.st", "TestSuite.st" };

        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(folder, classNames,
                ImageSuite.image());
        
        System.out.println("----------------------------------------------");
       
        STObject result = EvalSuite.evalFile(folder + "run.st");
        System.out.println("    Result: " + result.toString());
    }

    public void run() {
        testPrimitives();
        runSmalltalkTestSuite();
    }

    private void testPrimitives() {
        new PrimitiveTester().test();
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


