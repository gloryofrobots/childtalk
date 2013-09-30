package ua.ho.gloryofrobots.childtalk.stobject;

import java.io.IOException;
import java.io.ObjectOutputStream;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STContext extends STObject {
    private static final long serialVersionUID = 1L;
    STObject mReceiver;
    Routine mRoutine;
    protected STExecutableObject mSignalHandler = null;
    protected STObject mHandledSignal = ImageSuite.image().objects().NIL;
    protected STExecutableObject mEnsuredBlock;
    
    private STContext() {
        transformToScopedObject();
    }
    
    public static STContext create() {
        STContext obj = new STContext();
        obj.setClassProvider(new BindingClassProvider(obj) {
           
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().Context;
            }
        });
        return obj;
    }
    
    public void setRoutine(Routine routine) {
        mRoutine = routine;
    }
    
    public Routine getRouitne() {
        return mRoutine;
    }
    
    public STObject getReceiver() {
        return mReceiver;
    }
    
    public void setReceiver(STObject mReceiver) {
        this.mReceiver = mReceiver;
    }
    
    public void assign(STObject varName, STObject value) {
        if (mScope.assign(varName, value) == true) {
            return;
        }
        
        if(mReceiver.getScope().assign(varName, value) == true) {
            return;
        }
        
        if(mReceiver.getSTClass().getScope().assign(varName, value) == true) {
            return;
        }
    }

    public STObject lookup(STObject varName) {
        STObject result = mScope.lookup(varName);
        
        if(result == null) {
            if(mReceiver == null) {
                SignalSuite.error("mReceiver == null");
            }
            
            result = mReceiver.lookup(varName);
        }
        
        
        if(result == null) {
            return ImageSuite.image().lookup(varName);
        }
        
        return result;
    }
    
    public void pushScope(STScope scope) {
        scope.append(mScope);
        setScope(scope);
    }

    public STExecutableObject getSignalHandler() {
        return mSignalHandler;
    }

    public void setSignalHandler(STExecutableObject signalHandler) {
        this.mSignalHandler = signalHandler;
    }

    public STObject getHandledSignal() {
        return mHandledSignal;
    }

    public void setHandledSignal(STObject handledSignal) {
        this.mHandledSignal = handledSignal;
    }

    public STObject getParentContext() {
        if(mRoutine.getCaller() == null) {
            return ImageSuite.image().objects().NIL;
        }
        
        return mRoutine.getCaller().getContext();
    }

    public STObject getExecutable() {
        return mRoutine.getExecutable();
    }

    public int getCountArguments() {
        return mRoutine.getCountArguments();
    }

    public STProcess getProcess() {
        return mRoutine.getProcess();
    }

    public STExecutableObject getEnsuredBlock() {
        return mEnsuredBlock;
    }

    public void setEnsuredBlock(STExecutableObject ensuredBlock) {
        this.mEnsuredBlock = ensuredBlock;
    }
    
    private void writeObject(ObjectOutputStream oos)
            throws IOException {
                // default serialization 
            /*if(mRoutine != null) {
                throw new RuntimeException();
            }*/
                oos.defaultWriteObject();
                // write the object
              
            }
}
