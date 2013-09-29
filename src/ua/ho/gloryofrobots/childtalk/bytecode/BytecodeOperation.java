package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

public abstract class BytecodeOperation {
    public class BytecodeRuntimeError extends Exception {
        private static final long serialVersionUID = 1L;
        
    }
    
    protected Routine mRoutine;
    
    public void setRoutine(Routine routine) {
        mRoutine = routine;
    }
    
    public abstract void perform(int argument);

    public void runtimeError(String message, Object... args){
        
        String lineString = mRoutine.createErrorString();
        String errorMessage = "Runtime Error: " + message + " at line " + lineString;
        errorMessage += "\n" + DebugSuite.getTraceBackString(mRoutine);
        SignalSuite.error(errorMessage, args);
    }
}
