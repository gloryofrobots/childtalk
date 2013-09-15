package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;

//TODO BytecodeRoutine SuperClass
public class BlockRoutine extends MethodRoutine {
    public BlockRoutine(STExecutableObject executable) {
        super(executable);
    }

    @Override
    protected void onCompliteWithResult(STObject result) {
        STBlock block = (STBlock) mExecutable;
        Routine continuation = block.getContinuation();
        mProcess.completeRoutineWithResult(result, continuation);
    }
    
    @Override
    protected void createContext() {
        STBlock block = (STBlock) mExecutable;
        mContext = block.getContext();

        fillExecutableArguments();

        STScope scope = block.createScope();
        mContext.pushScope(scope);

    }
}
