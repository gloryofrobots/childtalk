package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.BlockRoutine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.ExceptionHandler;
import ua.ho.gloryofrobots.yellowtalk.scheduler.MethodRoutine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;

public class STBlock extends STExecutableObject implements ExceptionHandler {
    private static final long serialVersionUID = 1L;
    STContext mContext = STContext.create();
    Routine mContinuation;

    public void attachToRoutine(Routine routine) {
        mContinuation = routine;
        STContext context = mContinuation.getContext();
        mContext.setReceiver(context.getReceiver());
        mContext.setScope(context.getScope());
    }

    public STContext getContext() {
        return mContext;
    }

    public Routine getContinuation() {
        return mContinuation;
    }

    public static STBlock create() {
        STBlock block = new STBlock();
        block.setSTClass(Universe.classes().Block);

        return block;
    }

    @Override
    public void onException(STObject exception, Routine routine) {
        STStack stack = routine.getStack();
        stack.push(exception);
        SchedulingSuite.callExecutable(routine, this);
    }

    @Override
    public Routine createRoutine() {
        Routine routine = new BlockRoutine(this);
        return routine;
    }
}
