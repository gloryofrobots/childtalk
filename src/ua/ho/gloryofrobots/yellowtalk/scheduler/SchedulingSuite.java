package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class SchedulingSuite {
    private static Scheduler sScheduler;
    static {
        sScheduler = new Scheduler();
    }
    
    public static Scheduler scheduler() {
        return sScheduler;
    }
    
    public static void callForSelector(Routine caller, STObject receiver, STSymbol selector) {
        STClass superClass = receiver.getSTClass();
        STExecutableObject executable =  superClass.findMethod(selector);

        
        if (executable == null) {
            //TODO ST exception here
            SignalSuite.error("Unknown method %s in class %s",
                    selector.toString(),
                    superClass.toString());
        }
        
        callExecutable(caller, executable);
        
    }
    
    public static void callExecutable(Routine caller, STExecutableObject executable) {
        Routine routine = executable.createRoutine();
        routine.callFrom(caller);
    }
    
    public static void callExecutableWithExceptionHandling(Routine caller, STExecutableObject executable,
           STObject exception, ExceptionHandler handler) {
        Routine routine = executable.createRoutine();
        
        routine.setHandledException(exception);
        routine.setExceptionHandler(handler);
        routine.callFrom(caller);
    }
    
    public static STProcess callExecutableInNewProcess(STExecutableObject executable) {
        STProcess process = STProcess.create();
        
        //push default receiver for executable (may be create Eval executable for it)
        process.getStack().push(Universe.objects().NIL);
        process.callExecutable(executable);
        runProcess(process);
        return process;
    }
    
    public static void runProcess(STProcess process) {
        scheduler().addProcess(process);
        if(scheduler().isEnabled() == false) {
            scheduler().enable();
        }
    }
}
