package ua.ho.gloryofrobots.yellowtalk.scheduler;

import javax.management.RuntimeErrorException;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;

public class MethodRoutine extends Routine {
    
    protected int mInstructionPointer;
    protected int mLastInstructionIndex;

    public MethodRoutine(STExecutableObject executable) {
        super(executable);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void onActivate() {
        mInstructionPointer = 0;
        mLastInstructionIndex = mBytecode.getMaxSettedIndex();
    }

    protected void fillExecutableArguments() {
        int countArguments = mExecutable.getCountArguments();
        for (int i = 0; i < countArguments; i++) {
            STObject argValue = mArguments.at(i);
            mExecutable.addArgumentValue(i, argValue);
        }
    }
    
    @Override
    protected void createContext() {
        mContext = STContext.create();
    }
    
    @Override
    protected void initContext() {
        fillExecutableArguments();

        STObject receiver = mStack.pop();
        mContext.setReceiver(receiver);
        mContext.setRoutine(this);
        STScope scope = mExecutable.createScope();
        //STClass klass = receiver.getSTClass();
        scope.put(Universe.symbols().SELF, receiver);
        scope.put(Universe.symbols().SUPER, receiver);
        scope.put(Universe.symbols().THIS_CONTEXT, mContext);
        //scope.append(receiver.getScope());
        mContext.setScope(scope);
    }

    @Override
    protected void onCompliteWithResult(STObject result) {
        setReturnValue(result);
        complete();
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
        /*if (mInstructionPointer == mLastInstructionIndex) {
            onCompliteWithResult(getContext().getReceiver());
        }*/
    }
    
    @Override
    public String createErrorString() {
        int previousPointer = mInstructionPointer;
        String line = mExecutable.getCompileInfo().getCodeLine(previousPointer);
        line = "Error in" + mExecutable.toString() + " : " + line;
        return line;
    }

    //This important for block and method do nothing
    @Override
    protected void onExplicitCompleteWithResult(STObject result) {
        onCompliteWithResult(result);
    }
    
    @Override
    public String toString() {
        String data =  mExecutable.toString() + "( " + Integer.toHexString(this.hashCode()) + " ) ";
        if(mArguments != null && mArguments.size() > 0) {
            data += "( args:" + mArguments.toString() + " )";
        }
        return data;
    }
}
