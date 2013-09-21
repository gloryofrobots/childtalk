package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

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
        String errorMessage = "Runtime Error: " + message + " at line " + lineString + "\n bytecode :" 
                    + mRoutine.getExecutable().getBytecode().toString();
        DebugSuite.printTraceBackString(mRoutine);
        SignalSuite.error(errorMessage, args);
    }
}
