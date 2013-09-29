package ua.ho.gloryofrobots.childtalk.scheduler;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.childtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.childtalk.compilation.Compiler;
import ua.ho.gloryofrobots.childtalk.compilation.FileEvalException;
import ua.ho.gloryofrobots.childtalk.compilation.Lexer;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStream;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.node.EvalNode;
import ua.ho.gloryofrobots.childtalk.node.ProgramNode;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;

public class EvalSuite {
    public static STObject evalFile(String path) {
        ProgramTextStreamInterface programStream = Loader
                .createProgramStream(path);
        STExecutableObject executable = CompileSuite.compileEval(programStream);
        STProcess process = SchedulingSuite
                .callExecutableInNewProcess(executable, ImageSuite.image().objects().NIL);
        STObject result = process.getResult();

        return result;
    }
    
    public static void loadAndEvalFiles(String[] filesToRun) {
        Loader loader = new Loader();
        STImage image = ImageSuite.image();
        for(String path : filesToRun) {
            loader.loadAndCompileFile(path, image);
        }
    }
    public static STObject eval(String line) {
        ByteArrayInputStream wrap;
        String evaledLine = "Eval["+line+"]";
        
        try {
            wrap = new ByteArrayInputStream(evaledLine.getBytes("UTF-8"));
            ProgramTextStream stream = new ProgramTextStream(wrap);
            Lexer lexer = Lexer.create(stream);
            ProgramNode program = CompileSuite.parseWithLexer(lexer);
            EvalNode node = (EvalNode) program.getNode(0);

            Compiler compiler = new Compiler();
            STProcess process = compiler.compileAndExecuteEval(node);
            STObject object = process.getResult();

            return object;
        } catch (UnsupportedEncodingException | ProgramReadException e) {
            e.printStackTrace();
            SignalSuite.error("Error decoding evaled string");
        } catch (FileEvalException e) {
            //e.printStackTrace();
            SignalSuite.error(e.getMessage());
        } catch (ClassCastException e) {
            SignalSuite.error(e.getMessage());
        }

        return null;
    }
}
