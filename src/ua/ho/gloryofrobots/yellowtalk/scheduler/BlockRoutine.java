package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.stobject.STBlock;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STProcess;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;

//TODO BytecodeRoutine SuperClass
public class BlockRoutine extends MethodRoutine {
    public BlockRoutine(STExecutableObject executable) {
        super(executable);
    }
    
    @Override
    protected void onCompliteWithResult(STObject result) {
        super.onCompliteWithResult(result);
    }
    
    //This important for block and method do nothing
    @Override
    protected void onExplicitCompleteWithResult(STObject result) {
        STBlock block = (STBlock) getExecutable();
        Routine continuation = block.getContinuation();
        STProcess process = getProcess();
        
        process.explicitCompliteRoutineWithResult(result, continuation);
    }
    
    @Override
    protected void createContext() {
        STBlock block = (STBlock) mExecutable;
        mContext = block.getContext();
        mContext.setRouitne(this);
        fillExecutableArguments();

        STScope scope = block.createScope();
        mContext.pushScope(scope);

    }
}
