package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeArray;
import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeInterpreter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;
import ua.ho.gloryofrobots.yellowtalk.stobject.STStack;

//[2 > 0 ifTrue: [ 2 >1 ifTrue: [^3]. ^4]] value

public class Routine {
    protected STProcess mProcess;
    protected int mStackEnterPosition;
    protected int mInstructionPointer;
    protected int mLastInstructionIndex;
    protected STStack mStack;
    protected STExecutableObject mExecutable;
    protected Routine mCaller;
    protected BytecodeArray mBytecode;
    protected STContext mContext;

    public Routine(STExecutableObject executable) {
        mExecutable = executable;
        mBytecode = mExecutable.getBytecode();
    }
   

    private void createContext() {
        // STContext context = mExecutable.createContext();
        mContext = new STContext();
        int countArguments = mExecutable.getCountArguments();
        // FIXME CAREFUL ORDER
        for (int i = 1; i <= countArguments; i++) {
            STObject argValue = mStack.pop();
            mExecutable.addArgumentValue((countArguments - i), argValue);
        }

        STObject receiver = mStack.pop();
        mContext.setReceiver(receiver);

        STScope scope = mExecutable.createScope();
        scope.append(receiver.getScope());
        mContext.setScope(scope);
    }

    public void activate(STProcess process) {
        mProcess = process;
        mStack = process.getStack();
        process.setActiveRoutine(this);

        createContext();

        mStackEnterPosition = mStack.getCurrentPosition();
        mInstructionPointer = 0;
        mLastInstructionIndex = mBytecode.getSize();
    }
    
    public void callFrom(Routine caller) {
        if (mCaller != null) {
            // TODO ERROR
        }

        mCaller = caller;
        STProcess process = mCaller.getProcess();
        activate(process);
    }

    
    public void resume() {
        mProcess.setActiveRoutine(this);
    }

    public void terminate() {
        mProcess.killRoutine(this);
    }

    private void complete() {
        STObject returnValue = mStack.pop();
        mStack.setIndex(mStackEnterPosition);
        mStack.push(returnValue);
        // mStack.set(mStackEnterPosition, returnValue);

        terminate();
    }

    public void execute() {
        int code = mBytecode.get(mInstructionPointer);
        int high = mBytecode.getHigh(code);
        int low = mBytecode.getLow(code);
        BytecodeInterpreter.performOperation(high, low, this);
        mInstructionPointer++;
        if (mInstructionPointer >= mLastInstructionIndex) {
            complete();
        }
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
}
