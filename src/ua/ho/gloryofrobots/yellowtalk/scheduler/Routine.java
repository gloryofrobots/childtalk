package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeArray;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STArray;
import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSignal;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

//[2 > 0 ifTrue: [ 2 >1 ifTrue: [^3]. ^4]] value

public abstract class Routine {
    protected STProcess mProcess;
    
    protected STStack mStack;
    protected STExecutableObject mExecutable;
    protected boolean mIsComplete;
    protected Routine mCaller;
    protected Routine mCalled;
    
    protected BytecodeArray mBytecode;
    protected STContext mContext;
    
    protected STArray mArguments;
    
    protected ExceptionHandler mExceptionHandler;

    protected STObject mHandledException;
    protected int mStackEnterPosition;
    public Routine(STExecutableObject executable) {
        mExecutable = executable;
        mBytecode = mExecutable.getBytecode();
    }

    public void activate(STProcess process) {
        
        mProcess = process;
        mStack = process.getStack();
        catchArguments();
        process.setActiveRoutine(this);
        createContext();
        mStackEnterPosition = mStack.getCurrentPosition();
        onActivate();
    }
    
    protected abstract void createContext();
    
    public abstract void onActivate();
    
    public void call(STProcess process) {
        activate(process);
    }
    
    public void callFrom(Routine caller) {
        if (mCaller != null) {
            // TODO ERROR
            throw new RuntimeException();
        }
        
        mCaller = caller;
        caller.mCalled = this;
        
        STProcess process = mCaller.getProcess();
        activate(process);
    }

    public void resume() {
        mProcess.setActiveRoutine(this);
    }
    
    public void uncomplete() {
        mIsComplete = false;
    }
    
    public void complete() {
        //System.out.println("COMPLETE!!" + this.toString());
        mIsComplete = true;
    }
    
    public boolean isComplete() {
        //System.out.println("is COMPLETE!!" + this.toString());
        return mIsComplete;
    }
    /*
    protected void complete() {
        onComplete();

        // mStack.set(mStackEnterPosition, returnValue);
        terminate();
    }
    protected abstract void onComplete();
    */
    
    protected void setReturnValue(STObject value) {
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_SCHEDULER, "Routine returns %s", value.toString());
        mStack.setIndex(mStackEnterPosition);
        mStack.push(value);
    }

    public void execute() {
        if(isComplete()){
            SignalSuite.error("Routine  execute error: already complete");
        }
        
        onExecute();
    }
    
    public void explicitCompleteWithResult(STObject result) {
        onExplicitCompleteWithResult(result);
    }
    
    protected abstract void onExplicitCompleteWithResult(STObject result);
    protected abstract void onExecute();

    public STStack getStack() {
        return mStack;
    }

    public STExecutableObject getExecutable() {
        return mExecutable;
    }

    // TODO REMOVE TO METHOD
    public STContext getContext() {
        return mContext;
    }

    public STProcess getProcess() {
        return mProcess;
    }

    public void compliteWithResult(STObject result) {
        onCompliteWithResult(result);
    }
    
    public void setExceptionHandler(ExceptionHandler handler) {
        mExceptionHandler = handler;
    }

    public void setHandledException(STObject exception) {
        mHandledException = exception;
    }

    protected boolean handleException(STObject exception) {
        if(mHandledException != exception) {
            return false;
        }
        
        mExceptionHandler.onException(exception, this);
        return true;
    }
    protected abstract void onCompliteWithResult(STObject result);

    public STObject getArgument(int index) {
        if(index < 0 || index > mArguments.size()) {
            //FIXME signal
            throw new RuntimeException();
        }
        return mArguments.at(index);
    }

    public void flushArgumentsToStack(STStack stack) {
        int countArguments = mArguments.size();
        for (int i = 1; i <= countArguments; i++) {
            mStack.push(mArguments.at(countArguments - i));
        }
    }

    private void catchArguments() {
        int countArguments = mExecutable.getCountArguments();
        mArguments = STArray.create(countArguments);
        for (int i = 1; i <= countArguments; i++) {
            STObject argValue = mStack.pop();
            mArguments.put(countArguments - i, argValue);
        }
    }
    
    public void raise(STSignal signal) {
        throw new RuntimeException();
    }

    public Routine getCaller() {
        return mCaller;
    }
    
    @Override
    public String toString() {
        String data =  "<" + getClass().getSimpleName() + " " + Integer.toHexString(this.hashCode()) + mExecutable.toString();
        if(mArguments != null && mArguments.size() > 0) {
            data += "args:" + mArguments.toString();
        }
        data += ">";
        return data;
    }
    
    abstract public String createErrorString();
}
