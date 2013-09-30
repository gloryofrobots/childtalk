package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STSignal extends STObject {
  
    private static final long serialVersionUID = 1L;

    class StackTrace {

        public void add(Routine routine) {
            
        }
        
    }
    
    public void addToTrace(Routine routine) {
        mTrace.add(routine);
    }
    
    StackTrace mTrace;
    STSignal() {
        mTrace = new StackTrace();
        transformToScopedObject();
        
    }
    
    public static STSignal create() {
        STSignal obj = new STSignal();
        obj.setClassProvider(new BindingClassProvider(obj) {
          
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().Signal;
            }
        });
        
        return obj;
    }
}
