package ua.ho.gloryofrobots.childtalk.scheduler;

import ua.ho.gloryofrobots.childtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.childtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;

public class EvalSuite {
    public static STObject evalFile(String path) {
        ProgramTextStreamInterface programStream = Loader
                .createProgramStream(path);
        STExecutableObject executable = CompileSuite.compileEval(programStream);
        STProcess process = SchedulingSuite
                .callExecutableInNewProcess(executable);
        STObject result = process.getResult();

        return result;
    }
}
