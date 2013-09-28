package ua.ho.gloryofrobots.childtalk.scheduler;

import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STMethod;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STPrimitive;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

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
        mContext.setRoutine(this);
    }
    
    @Override
    protected void initContext() {
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
            onFailed();
        } 
    }
    
 // Primitive failed. Execute method bytecodes
    private void onFailed() {
        mFailed = true;
        Routine routine = new MethodRoutine(mExecutable);
        STObject receiver = mContext.getReceiver();
        mStack.push(receiver);
        flushArgumentsToStack(mStack);
        routine.callFrom(this);
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

    @Override
    public Routine getLastMethodRoutine() {
        return getCaller().getLastMethodRoutine();
    }
}
