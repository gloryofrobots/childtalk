package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.scheduler.Routine;

public class STPrimitiveProxy extends STPrimitive {
    
    private static final long serialVersionUID = 1L;

    public interface Callback {
        STObject call(STObject receiver, STObject... arguments);
    }

    private int mCountArguments;
    private Callback mCallback;

    STPrimitiveProxy(int countArguments, Callback callback) {
        mCountArguments = countArguments;
        mCallback = callback;
    }

    private STObject callArg0(STObject receiver, STStack stack) {
        return mCallback.call(receiver);
    }

    private STObject callArg1(STObject receiver, STStack stack) {
        STObject arg1 = stack.pop();
        return mCallback.call(receiver, arg1);
    }

    @Override
    protected STObject onExecute(Routine routine, STObject receiver,
            STStack stack) {
        switch (mCountArguments) {
        case 0:
            return callArg0(receiver, stack);
        case 1:
            return callArg1(receiver, stack);
        }
        
        return null;
    }

}
