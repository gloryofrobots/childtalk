package ua.ho.gloryofrobots.yellowtalk.scheduler;

import javax.management.RuntimeErrorException;

import ua.ho.gloryofrobots.yellowtalk.bytecode.BytecodeInterpreter;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;

public class MethodRoutine extends Routine {
    protected int mStackEnterPosition;
    protected int mInstructionPointer;
    protected int mLastInstructionIndex;
    
    public MethodRoutine(STExecutableObject executable) {
        super(executable);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void onActivate() {
        createContext();

        mStackEnterPosition = mStack.getCurrentPosition();
        mInstructionPointer = 0;
        mLastInstructionIndex = mBytecode.getSize();
    }
    
    protected void fillExecutableArguments() {
        int countArguments = mExecutable.getCountArguments();
        // FIXME CAREFUL ORDER
        for (int i = 1; i <= countArguments; i++) {
            STObject argValue = mArguments.get(i - 1);
            mExecutable.addArgumentValue((countArguments - i), argValue);
        }
    }
    
    protected void createContext() {
        // STContext context = mExecutable.createContext();
        mContext = new STContext();
        fillExecutableArguments();

        STObject receiver = mStack.pop();
        mContext.setReceiver(receiver);

        STScope scope = mExecutable.createScope();
        scope.append(receiver.getScope());
        mContext.setScope(scope);
    }
    
    @Override
    protected void onCompliteWithResult(STObject result) {
        mStack.setIndex(mStackEnterPosition);
        setReturnValue(result);
        terminate();
    }
        
    @Override
    protected void onExecute() {
        if (mInstructionPointer >= mLastInstructionIndex) {
            throw new RuntimeException();
        }
        
        int code = mBytecode.get(mInstructionPointer);
        int high = mBytecode.getHigh(code);
        int low = mBytecode.getLow(code);
        BytecodeInterpreter.performOperation(high, low, this);
        mInstructionPointer++;
    }
}
