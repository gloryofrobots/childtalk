package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.LinkedList;

import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;

public class STProcess extends STObject {
    private static final long serialVersionUID = 1L;
    LinkedList<Routine> mCallStack;
    State mState;
    STStack mStack;
    
    public enum State {
        TERMINATED, SUSPENDED, ACTIVE, IDLE;
    }
    
    public STProcess() {
        mState = State.IDLE;
        mCallStack = new LinkedList<Routine>();
        mStack = new STStack();
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
    
    public void setActiveRoutine(Routine routine) {
        if(mCallStack.size() > 0 
                && mCallStack.getLast().equals(routine)) {
            return;
        }
        
        mCallStack.add(routine);
        //routine.setProcess(this);
    }
    
    public void execute() {
        if(mCallStack.size() == 0) {
            setState(State.TERMINATED);
            return;
        }
        
        mCallStack.getLast().execute();
    }

    public void killRoutine(Routine routine) {
        if(mCallStack.getLast().equals(routine) == false) {
            //TODO THROW ERROR
            return;
        }
        
        mCallStack.removeLast();
    }
}
