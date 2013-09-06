package ua.ho.gloryofrobots.yellowtalk.bytecode;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

public abstract class BytecodeOperation {
    public class BytecodeRuntimeError extends Exception {
        private static final long serialVersionUID = 1L;
        
    }
    public void setRoutine(Routine routine) {
        mRoutine = routine;
    }
    
    abstract void perform(int argument) throws BytecodeRuntimeError;

    protected Routine mRoutine;
    
    public void runtimeError(String message, Object... args) throws BytecodeRuntimeError{
        
    }
}
