package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.Universe;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.scheduler.BlockRoutine;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STBlock extends STExecutableObject {
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
        block.setClassProvider(new BindingClassProvider(block) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Block;
            }
        });
        
        return block;
    }
    
   
   
    @Override
    public Routine createRoutine() {
        Routine routine = new BlockRoutine(this);
        return routine;
    }
    
    @Override
    public String toString() {
        return String.format("<Block : %s>", getCompileInfo().toString());
    }

    public STBlock copySelf() {
        STBlock block = STBlock.create();
        try {
            super.fillExecutable(block);
        } catch (DuplicateVariableException e) {
            return null;
        }
        
        return block;
    }
}
