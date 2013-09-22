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
        // DebugSuite.setDebugMode(DebugSuite.DEBUG_MODE_COMPILER,
        // DebugSuite.DEBUG_MODE_INTERPRETER);
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
        String[] classNames = { "TestCase", "ObjectTest", "FalseTest",
                "UndefinedObjectTest", "TrueTest", "CommonTest", "TestSuite" };

        Loader loader = new Loader();
        loader.loadAndCompileClassesFromFolder(folder, classNames,
                Universe.image());
        System.out.println("----------------------------------------------");
        /*
         * BootstrapSuite .evalFile(
         * "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/tests/compiler_test.st"
         * );
         */
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

//
// ret_obj = _interpret ("method ");
// assert (SYX_SMALL_INTEGER(ret_obj) == -123 + 0x2AE + -4);
//
// return 0;
//
// puts ("- Test ifTrue:ifFalse: again");
// ret_obj = _interpret ("method ^true ifTrue: [ 123 ] ifFalse: [ 321 ]");
// int res = SYX_SMALL_INTEGER(ret_obj);
// assert (res == 123);
//
//
//
// puts ("- Test floats");
// ret_obj = _interpret ("method | a | a := 123.321. ^a + 2.2");
// check = SYX_OBJECT_FLOAT(ret_obj);
// /*assert (check == 125.521);*/
//
// #ifdef HAVE_LIBGMP
// puts ("- Test large integers");
// ret_obj = _interpret ("method ^16rFFFFFFFFFFFF - 16rFFFFFFFFFFF0");
// assert (SYX_SMALL_INTEGER(ret_obj) == 0xF);
// /* 30 bits + 30 bits */
// ret_obj = _interpret
// ("method ^2r1111111111111111111111111111111 + 2r1111111111111111111111111111111 = 4294967294");
// assert (SYX_IS_TRUE (ret_obj));
// #endif
//
// puts ("- Test class variables");
// lexer = syx_lexer_new
// ("Object subclass: #TestClass instanceVariableNames: '' classVariableNames: 'TestVar'!"
// "!TestClass class methodsFor: 'testing'!"
// "initialize TestVar := 123! testVar ^TestVar ! !"
// "!TestClass methodsFor: 'testing'!"
// "testVar ^TestVar ! !");
// ok = syx_cold_parse (lexer);
// assert (ok == TRUE);
// syx_lexer_free (lexer, FALSE);
//
// ret_obj = _interpret
// ("method TestClass initialize. ^TestClass testVar + TestClass new testVar");
// assert (SYX_SMALL_INTEGER(ret_obj) == 123 + 123);
//
// puts ("- Test evaluating a simple block");
// ret_obj = _interpret ("method ^[321] value");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test block stack return");
// ret_obj = _interpret ("method [^321] value");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test a block with a single argument");
// ret_obj = _interpret ("method [:s | ^s] value: 123");
// assert (SYX_SMALL_INTEGER(ret_obj) == 123);
//
// puts ("- Test temporary scope in blocks");
// ret_obj = _interpret
// ("method | tmp | tmp := 123. [ | tmp | tmp := 321 ] value. ^tmp");
// assert (SYX_SMALL_INTEGER(ret_obj) == 123);
//
// puts ("- Another test for temporary scope in blocks");
// ret_obj = _interpret
// ("method | tmp | tmp := 123. [ tmp := 321 ] value. ^tmp");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test nested blocks with arguments");
// ret_obj = _interpret ("method ^[ :s | [ :s | s ] value: 321] value: 123");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Another test for nested blocks and arguments");
// ret_obj = _interpret ("method | b | b := [ :a | a ]."
// "^[ :b | "
// "   ([ :b | "
// "      ([ :b | b value: 123 + 1 ] value: b) + 1"
// "   ] value: b) + 1"
// "] value: b");
// assert (SYX_SMALL_INTEGER(ret_obj) == 126);
//
// puts ("- Recursive blocks");
// ret_obj = _interpret
// ("method | b i | i := 0. b := [ :b | (i := i + 1) = 10 ifFalse: [ b value: b ] ]."
// "b value: b. ^i");
// assert (SYX_SMALL_INTEGER(ret_obj) == 10);
//
// puts ("- Test ifTrue:");
// ret_obj = _interpret
// ("method | var | var := 123. var = 321 ifTrue: [^false]. var = 123 ifTrue: [^true]");
// assert (SYX_IS_TRUE (ret_obj));
//
// puts ("- Test ifTrue:ifFalse:");
// ret_obj = _interpret ("method ^false ifTrue: [ 123 ] ifFalse: [ 321 ]");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test ifTrue:ifFalse: again");
// ret_obj = _interpret ("method ^true ifTrue: [ 123 ] ifFalse: [ 321 ]");
// assert (SYX_SMALL_INTEGER(ret_obj) == 123);
//
// puts ("- Test ifFalse:ifTrue:");
// ret_obj = _interpret
// ("method ^true ifTrue: [ 123 = 321 ifFalse: [^333] ifTrue: [^222] ] ifFalse: [^false]");
// assert (SYX_SMALL_INTEGER(ret_obj) == 333);
//
// puts ("- Test temporaries in optimized blocks");
// ret_obj = _interpret
// ("method | tmp | tmp := 123. true ifTrue: [ | tmp | tmp := 321 ]. ^tmp");
// assert (SYX_SMALL_INTEGER(ret_obj) == 123);
//
// /* From issue #29 */
// puts ("- Test block recursion");
// _interpret
// ("method | b | b := [ :i | | d | Transcript nextPutAll: 'before ', i printString; cr."
// "d := i - 1."
// "(i > 0 ) ifTrue: [ b value: d]."
// "Transcript nextPutAll: 'after ', i printString; cr. ]."
// "b value: 5");
//
// puts ("- Test exception handling");
// ret_obj = _interpret ("method ^[Signal signal] on: Signal do: [:ex | true]");
// assert (SYX_IS_TRUE (ret_obj));
//
// puts ("- Test resuming");
// ret_obj = _interpret
// ("method ^[Signal signal. 123] on: Signal do: [ :ex | ex resume. 321]");
// assert (SYX_SMALL_INTEGER(ret_obj) == 123);
//
// puts ("- Test pass");
// ret_obj = _interpret
// ("method ^[[Error signal. 123] on: Error do: [ :ex | ex pass. 213]] on: Exception do: [:ex | 321]");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test ensuring");
// ret_obj = _interpret ("method [Signal signal. 123] ensure: [^321]. 213");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test ensuring 2");
// ret_obj = _interpret ("method [^123] ensure: [^321]. 213");
// assert (SYX_SMALL_INTEGER(ret_obj) == 321);
//
// puts ("- Test loops");
// ret_obj = _interpret
// ("method | var | 1 to: 1000 do: [ :i | var := i. 'test' print ]. ^var");
// assert (SYX_SMALL_INTEGER(ret_obj) == 1000);
