package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STProcess extends STObject {
    private static final long serialVersionUID = 1L;
    Routine mActiveRoutine;
    State mState;
    STStack mStack;

    public enum State {
        TERMINATED, SUSPENDED, ACTIVE, IDLE;
    }

    public STProcess() {
        mState = State.IDLE;
        mStack = STStack.create();
    }

    public static STProcess create() {
        STProcess obj = new STProcess();
        obj.setClassProvider(new BindingClassProvider(obj) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Process;
            }
        });
        return obj;
    }

    public STStack getStack() {
        return mStack;
    }

    public State getState() {
        return mState;
    }
    
  
    
    private void setState(State state) {
        mState = state;
    }

    public void callExecutable(STExecutableObject executable) {
        Routine routine = executable.createRoutine();
        routine.call(this);
    }

    public void setActiveRoutine(Routine routine) {

        if (mActiveRoutine == routine) {
            return;
        }

        mActiveRoutine = routine;
        // routine.setProcess(this);
    }

    public void execute() {
        findRoutineToExecute();

        if (mActiveRoutine == null) {
            terminate();
        } else {
            DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_SCHEDULER,
                    "Process run \n%s", mActiveRoutine.toString());

            mActiveRoutine.execute();
        }
    }

    private void findRoutineToExecute() {
        Routine routine = mActiveRoutine;
        while (routine != null && routine.isComplete() == true) {
            // routine.uncomplete();
            routine = routine.getCaller();
        }
        mActiveRoutine = routine;
    }

    public void killRoutine(Routine routine) {
        mActiveRoutine = mActiveRoutine.getCaller();
    }

    public void explicitCompliteRoutineWithResult(STObject result,
            Routine continuation) {
        Routine top = mActiveRoutine;
        while (top != continuation) {
            if (top == null) {
                // TODO exception
                SignalSuite.error("Continuation not exist");
            }
            top = top.getCaller();
        }

        mActiveRoutine = top;
        continuation.explicitCompleteWithResult(result);
    }

    public boolean isTerminated() {
        return mState == State.TERMINATED;
    }

    public void terminate() {
        setState(State.TERMINATED);
    }

    public boolean isSuspended() {
        return mState == State.SUSPENDED;
    }

    public void suspend() {
        setState(State.SUSPENDED);
    }

    public boolean isActive() {
        return mState == State.ACTIVE;
    }

    public void activate() {
        setState(State.ACTIVE);
    }

    public STObject getResult() {
        if (mStack.getCurrentIndex() != 0) {
            return Universe.objects().NIL;
        }

        return mStack.peek();
    }
    
    public void terminateFromRoutine(Routine routine) {
        DebugSuite.printTraceBackString(routine);
        terminate();
    }
    
    public void raiseFromRoutine(Routine routine, STObject signal) {    
        Routine current = routine;
        while (current != null) {
            if (current.canHandleSignal(signal)) {
                setActiveRoutine(current);
                current.handleSignal(signal);
                return;
            }

            current = current.getCaller();
        }

        onUnhandledSignal(routine, signal);
    }

    private void onUnhandledSignal(Routine routine, STObject signal) {
        //TODO Process name. Good TraceBack
        SignalSuite.warning("Unhandled signal");
       
        DebugSuite.printTraceBackString(mActiveRoutine);
        terminate();
    }
}
