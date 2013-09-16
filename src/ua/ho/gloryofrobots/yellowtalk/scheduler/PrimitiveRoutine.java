package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STContext;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STMethod;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STPrimitive;
import ua.ho.gloryofrobots.yellowtalk.stobject.STSymbol;

public class PrimitiveRoutine extends Routine {
    private STPrimitive mPrimitive;
    private boolean mFailed;

    public PrimitiveRoutine(STExecutableObject executable) {
        super(executable);
    }

    @Override
    public void onActivate() {
        STSymbol primitiveName = ((STMethod) mExecutable).getPrimitiveName();

        if (primitiveName == null) {
            mFailed = true;
            // TODO exception
            throw new RuntimeException();
        }

        STObject receiver = mContext.getReceiver();
        
        
        STClass klass = null;
        if (receiver instanceof STClass) {
            klass = (STClass) receiver;
            mPrimitive = klass.getPrimitive(primitiveName);
        } 
        if(mPrimitive == null) {
            klass = mContext.getReceiver().getSTClass();
            mPrimitive = klass.getPrimitive(primitiveName);
        }
        
        if (mPrimitive == null) {
            mFailed = true;
            // TODO Smalltalk exception
            throw new RuntimeException();
        }
    }

    @Override
    protected void createContext() {
        mContext = STContext.create();
        STObject receiver = mStack.pop();
        mContext.setReceiver(receiver);
    }

    @Override
    protected void onExecute() {
        // we already failed and method was called.
        if (mFailed) {
            terminate();
        }

        if (mPrimitive.execute(this) == false) {
            mFailed = true;
            // Primitive failed. Execute method bytecodes
            Routine routine = new MethodRoutine(mExecutable);
            routine.callFrom(this);
        } else {
            terminate();
        }
    }

    @Override
    public String createErrorString() {
        return "Primitive";
    }

    @Override
    protected void onCompliteWithResult(STObject result) {
        SignalSuite
                .error("Error in interpreter logic onCompliteWithResult of PrimitiveRoutine should not called");
    }
}
