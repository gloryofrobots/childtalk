package ua.ho.gloryofrobots.yellowtalk.stobject;


import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
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
        mStack = new STStack();
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
        if (mActiveRoutine == null) {
            terminate();
            return;
        }

        mActiveRoutine.execute();
    }

    public void killRoutine(Routine routine) {
        if (mActiveRoutine.equals(routine) == false) {
            throw new RuntimeException();
        }

        mActiveRoutine = mActiveRoutine.getCaller();
    }

    public void completeRoutineWithResult(STObject result, Routine continuation) {
        Routine top = mActiveRoutine;
        while (top != continuation) {
            top = mActiveRoutine.getCaller();
        }

        continuation.compliteWithResult(result);
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
        if(mStack.getCurrentIndex() != 0) {
            return null;
        }
        
        return mStack.peek();
    }
}
