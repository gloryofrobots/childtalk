package ua.ho.gloryofrobots.yellowtalk.scheduler;

import javax.management.RuntimeErrorException;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
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
        mStackEnterPosition = mStack.getCurrentPosition();
        mInstructionPointer = 0;
        mLastInstructionIndex = mBytecode.getMaxSettedIndex();
    }

    protected void fillExecutableArguments() {
        int countArguments = mExecutable.getCountArguments();
        // FIXME CAREFUL ORDER
        for (int i = 1; i <= countArguments; i++) {
            STObject argValue = mArguments.get(i - 1);
            mExecutable.addArgumentValue((countArguments - i), argValue);
        }
    }
    
    @Override
    protected void createContext() {
        // STContext context = mExecutable.createContext();
        mContext = STContext.create();
        fillExecutableArguments();

        STObject receiver = mStack.pop();
        mContext.setReceiver(receiver);

        STScope scope = mExecutable.createScope();
        scope.put(Universe.symbols().SELF, receiver);
        //scope.append(receiver.getScope());
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
            SignalSuite
                    .error("Illegal MethodRoutine state : instruciton pointer larger than total instructions length %d %d",
                            mInstructionPointer, mLastInstructionIndex);
        }

        short high = mBytecode.getHigh(mInstructionPointer);
        int low = mBytecode.getLow(mInstructionPointer);
        InterpreterSuite.performOperation(high, low, this);
        mInstructionPointer++;
    }
    
    @Override
    public String createErrorString() {
        int previousPointer = mInstructionPointer;
        String line = mExecutable.getCompileInfo().getCodeLine(previousPointer);
        return line;
    }
    
}
