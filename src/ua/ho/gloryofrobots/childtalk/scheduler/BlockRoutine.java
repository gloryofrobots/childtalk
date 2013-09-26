package ua.ho.gloryofrobots.childtalk.scheduler;

import ua.ho.gloryofrobots.childtalk.stobject.STBlock;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;
import ua.ho.gloryofrobots.childtalk.stobject.STScope;

//TODO BytecodeRoutine SuperClass
public class BlockRoutine extends MethodRoutine {
    private boolean mStackReturnEnabled = true;
    
    public BlockRoutine(STExecutableObject executable) {
        super(executable);
    }
    
    protected boolean pushEnsured(STObject result) {
        STExecutableObject ensuredBlock = mContext.getEnsuredBlock();
        if(ensuredBlock == null) {
            return false;
        }
        
        
        //push ensured only once.
        mContext.setEnsuredBlock(null);
        //we need to perform  return again
        mStack.push(result);
        mInstructionPointer--;
        BlockRoutine ensuredRoutine = (BlockRoutine) SchedulingSuite.callExecutable(this, ensuredBlock);
        ensuredRoutine.disableStackReturn();
        return true;
    }
    
    @Override
    protected void onCompliteWithResult(STObject result) {
        //push ensured block if we have it
        if(pushEnsured(result) == true) {
            return;
        }
        if(isEnabledStackReturn() == true) {
            //normal execution push result to stack and complete
            setReturnValue(result);
            complete();
        } else {
            //Ensured result is now in stack. 
            //for example [123] ensure:[321] must return 123.
            //rewind stack to start position without pushing result
            mStack.setIndex(mStackEnterPosition);
            complete();
        }
    }

    // Bad Design -> This important only for blocks
    @Override
    protected void onExplicitCompleteWithResult(STObject result) {
        //if we have ensured block than we don`t need to return from it`s context
        if(pushEnsured(result) == true) {
            return;
        }
        
        STBlock block = (STBlock) getExecutable();
        Routine continuation = block.getContinuation();
        STProcess process = getProcess();

        process.explicitCompliteRoutineWithResult(result, continuation);
    }

    @Override
    protected void createContext() {
        STBlock block = (STBlock) mExecutable;
        mContext = block.getContext();
        mContext.setRoutine(this);
    }

    @Override
    protected void initContext() {
        STBlock block = (STBlock) mExecutable;
        fillExecutableArguments();

        STScope scope = block.createScope();
        mContext.pushScope(scope);

    }
    
    public void disableStackReturn() {
        mStackReturnEnabled = false;
    }

    public boolean isEnabledStackReturn() {
        return mStackReturnEnabled;
    }

}
