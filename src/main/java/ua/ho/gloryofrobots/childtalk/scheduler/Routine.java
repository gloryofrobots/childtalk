package ua.ho.gloryofrobots.childtalk.scheduler;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.bytecode.BytecodeArray;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STArray;
import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;

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

    protected int mStackEnterPosition;

    public Routine(STExecutableObject executable) {
        mExecutable = executable;
        mBytecode = mExecutable.getBytecode();
        createContext();
    }

    public void activate(STProcess process) {

        mProcess = process;
        mStack = process.getStack();
        catchArguments();
        process.setActiveRoutine(this);
        initContext();
        mStackEnterPosition = mStack.getCurrentPosition();
        onActivate();
    }

    protected abstract void createContext();

    protected abstract void initContext();

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
        // System.out.println("COMPLETE!!" + this.toString());
        mIsComplete = true;
    }

    public boolean isComplete() {
        // System.out.println("is COMPLETE!!" + this.toString());
        return mIsComplete;
    }

    /*
     * protected void complete() { onComplete(); //
     * mStack.set(mStackEnterPosition, returnValue); terminate(); }
     * 
     * protected abstract void onComplete();
     */

    protected void setReturnValue(STObject value) {
        DebugSuite.debugPrint(DebugSuite.DEBUG_MODE_SCHEDULER,
                "Routine returns %s", value.toString());
        mStack.setIndex(mStackEnterPosition);
        mStack.push(value);
    }

    public void compliteWithResult(STObject result) {
        onCompliteWithResult(result);
    }

    public void execute() {
        if (isComplete()) {
            SignalSuite.error("Routine  execute error: already complete");
        }

        onExecute();
    }

    public void explicitCompleteWithResult(STObject result) {
        onExplicitCompleteWithResult(result);
    }

    public STStack getStack() {
        return mStack;
    }

    public STExecutableObject getExecutable() {
        return mExecutable;
    }

    public STContext getContext() {
        return mContext;
    }

    public STProcess getProcess() {
        return mProcess;
    }

    public STObject getArgument(int index) {
        if (index < 0 || index > mArguments.size()) {
            // FIXME signal
            throw new RuntimeException();
        }

        return mArguments.at(index);
    }

    public void flushArgumentsToStack(STStack stack, int first, int last) {
        for (int i = first; i < last; i++) {
            mStack.push(mArguments.at(i));
        }
    }

    public void flushArgumentsToStack(STStack stack) {
        flushArgumentsToStack(stack, 0, mArguments.size());
    }

    private void catchArguments() {
        int countArguments = mExecutable.getCountArguments();
        mArguments = STArray.create(countArguments);
        for (int i = 1; i <= countArguments; i++) {
            STObject argValue = mStack.pop();
            mArguments.put(countArguments - i, argValue);
        }
    }

    public Routine getCaller() {
        return mCaller;
    }

    // /SIGNALS
    public void initSignalHandling(STObject signal, STExecutableObject handler,
            STExecutableObject ensuredBlock) {
        mContext.setHandledSignal(signal);
        mContext.setSignalHandler(handler);
        mContext.setEnsuredBlock(ensuredBlock);
    }

    public boolean canHandleSignal(STObject signalInstance) {
        /*
         * if(mContext == null) { return false; }
         */
        STObject handledSignal = mContext.getHandledSignal();
        STObject signal = signalInstance.getSTClass();
        if (handledSignal != signal) {
            return false;
        }

        return true;
    }

    public void handleSignal(STObject signal) {
        STObject handledSignal = mContext.getHandledSignal();
        mStack.push(handledSignal);

        STExecutableObject signalHandler = mContext.getSignalHandler();
        SchedulingSuite.callExecutable(this, signalHandler);
    }

    public void raise(STObject receiver) {
        mProcess.raiseFromRoutine(this, receiver);
    }

    /*
     * protected boolean pushEnsured() { STExecutableObject ensuredBlock =
     * mContext.getEnsuredBlock(); if(ensuredBlock == null) { return false; }
     * 
     * SchedulingSuite.callExecutable(this, ensuredBlock); //push ensured only
     * once. mContext.setEnsuredBlock(null); //we need to perform explicit
     * return again onPushEnsured(); return true; }
     */

    // protected abstract void onPushEnsured();

    @Override
    public String toString() {
        String data = "<" + getClass().getSimpleName() + " "
                + Integer.toHexString(this.hashCode()) + mExecutable.toString();
        if (mArguments != null && mArguments.size() > 0) {
            data += "args:" + mArguments.toString();
        }
        data += ">";
        return data;
    }

    public int getCountArguments() {
        return mExecutable.getCountArguments();
    }

    protected abstract void onExplicitCompleteWithResult(STObject result);

    protected abstract void onExecute();

    protected abstract void onCompliteWithResult(STObject result);

    public abstract String createErrorString();

    // search method routine in routines call chain.
    // it`s necessary for searching context for dispatching messages to super
    public abstract Routine getLastMethodRoutine();

}
