package ua.ho.gloryofrobots.childtalk.scheduler;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

public class SchedulingSuite {
    private static Scheduler sScheduler;
    static {
        sScheduler = new Scheduler();
    }

    public static Scheduler scheduler() {
        return sScheduler;
    }

    public static Routine callForSelector(Routine caller, STClass klass,
            STObject selector) {
        
        STExecutableObject executable = klass.findMethod(selector);
        
        if (executable == null) {
            // TODO ST exception here
            klass.findMethod(selector);
            DebugSuite.printTraceBackString(caller);
            SignalSuite.error("Unknown method %s in class %s",
                    selector.toString(), klass.toString());
            SignalSuite.raiseError(caller, "Unknown method %s in class %s",
                    selector.toString(), klass.toString());
            return null;
        }
        
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_INTERPRETER, "Execute method %s", executable.toString());
        return callExecutable(caller, executable);

    }

    public static Routine callForSelectorWithArguments(Routine caller,
            STObject receiver, STSymbol selector, STObject... args) {
        STStack stack = caller.getStack();

        for (STObject arg : args) {
            stack.push(arg);
        }

        STClass superClass = receiver.getSTClass();
        STExecutableObject executable = superClass.findMethod(selector);

        if (executable == null) {
            SignalSuite.error("Unknown method %s in class %s",
                    selector.toString(), superClass.toString());
        }

        return callExecutable(caller, executable);
    }

    public static Routine callExecutable(Routine caller,
            STExecutableObject executable) {
        Routine routine = executable.createRoutine();
        routine.callFrom(caller);
        return routine;
    }

    public static void callExecutableWithExceptionHandling(Routine caller,
            STExecutableObject executable, STObject signal,
            STExecutableObject handler, STExecutableObject ensured) {

        Routine routine = executable.createRoutine();
        routine.initSignalHandling(signal, handler, ensured);
        routine.callFrom(caller);
    }

    public static STProcess callExecutableInNewProcess(
            STExecutableObject executable, STObject receiver) {
        STProcess process = STProcess.create();

        // push default receiver for executable (may be create Eval executable
        // for it)
        process.getStack().push(receiver);
        process.callExecutable(executable);
        runProcess(process);
        return process;
    }
    
    public static STProcess callSelectorInNewProcess(
            STObject selector, STClass klass, STObject receiver) {
        STExecutableObject executable = klass.findMethod(selector);
        return callExecutableInNewProcess(executable, receiver);
    }
    
    public static void runProcess(STProcess process) {
        scheduler().addProcess(process);
        if (scheduler().isEnabled() == false) {
            scheduler().enable();
        }
    }
}
