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
    private STSymbol mPrimitiveName;
    public PrimitiveRoutine(STExecutableObject executable) {
        super(executable);
    }

    @Override
    public void onActivate() {
        mPrimitiveName = ((STMethod) mExecutable).getPrimitiveName();

        if (mPrimitiveName == null) {
            mFailed = true;
            // TODO exception
            throw new RuntimeException();
        }

        STMethod method = (STMethod) mExecutable;
        
        mPrimitive =  method.getPrimitive();
        /*SignalSuite.warning("Primitive %s ", mPrimitiveName.toString());*/
        if(mPrimitive == null) {
            STObject receiver = mContext.getReceiver();
            STClass klass = receiver.getSTClass();
            mPrimitive = klass.getPrimitive(mPrimitiveName);
        }
        
        if (mPrimitive == null) {
            mFailed = true;
            SignalSuite.error("Primitive %s not exist", mPrimitiveName.toString());
            // TODO Smalltalk exception
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
            complete();
        }

        if (mPrimitive.execute(this) == false) {
            mFailed = true;
            // Primitive failed. Execute method bytecodes
            Routine routine = new MethodRoutine(mExecutable);
            routine.callFrom(this);
        } 
    }

    @Override
    public String createErrorString() {
        return "Primitive";
    }
    
    @Override
    public String toString() {
        String data =  "<PrimitiveRoutine :" + mPrimitiveName.toString() + ">";        
        //String data =  "<PrimitiveRoutine :" + mPrimitiveName.toString() + ">";        
        return data;
    }
    
    @Override
    protected void onCompliteWithResult(STObject result) {
        setReturnValue(result);
        complete();
        /**/
    }

    @Override
    protected void onExplicitCompleteWithResult(STObject result) {
        SignalSuite
        .error("Error in interpreter logic onExplicitCompleteWithResult of PrimitiveRoutine should not called");
    }
}
