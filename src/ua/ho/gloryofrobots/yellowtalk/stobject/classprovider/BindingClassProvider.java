package ua.ho.gloryofrobots.yellowtalk.stobject.classprovider;

import ua.ho.gloryofrobots.yellowtalk.stobject.STClass;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STScope;

public abstract class BindingClassProvider implements ClassProvider {
    private STObject mObject;
    private STClass mClass;
    
    public BindingClassProvider(STObject object) {
        mObject = object;
    }
    
    @Override
    public STClass getSTClass() {
        if(mClass != null) {
            return mClass;
        }
        
        STClass klass = _getSTClass();
        if(klass == null) {
            return null;
        }
        
        //STScope scope = klass.getScope();
        //mObject.getScope().append(scope);
        return klass;
    }
    
    protected abstract STClass _getSTClass();
}