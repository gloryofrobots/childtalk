package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.bootstrap.Loader;
import ua.ho.gloryofrobots.yellowtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;

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
