package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;

public abstract class STPrimitive extends STObject {

    private static final long serialVersionUID = 1L;

    public boolean execute(Routine routine) {
        STObject receiver = routine.getContext().getReceiver();
        STStack stack = routine.getStack();
        try {
            STObject result = onExecute(routine, receiver, stack);
            if (result == null) {
                return false;
            }
            // we don`t need write result to stack
            if (result == ImageSuite.image().objects().NIL) {
                routine.complete();
                return true;
            }

            routine.compliteWithResult(result);
            return true;
        } catch (Exception e) {
            //throw e;
            return false;
        }

    }

    public void primitiveError(Routine routine, String format, Object... args) {
        SignalSuite.error(format, args);
        // SignalSuite.raiseError(routine, format, args);
    }

    protected abstract STObject onExecute(Routine routine, STObject receiver,
            STStack stack);
}
