package ua.ho.gloryofrobots.childtalk.stobject;

import java.io.IOException;
import java.io.ObjectOutputStream;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.scheduler.BlockRoutine;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STBlock extends STExecutableObject {
    private static final long serialVersionUID = 1L;
    
    STContext mContext;
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
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().Block;
            }
        });
        
        return block;
    }
    
    protected STBlock() {
        mContext = STContext.create();
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
    private void writeObject(ObjectOutputStream oos)
            throws IOException {
                // default serialization 
            if(mContinuation != null) {
                throw new RuntimeException();
            }
                oos.defaultWriteObject();
                // write the object
              
            }
}
