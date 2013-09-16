package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STProtoObject extends STMetaclass {
    private static final long serialVersionUID = 1L;
    private static STProtoObject sInstance;

    private STProtoObject() {
        super();
    }

    public static STProtoObject get() {
        if (sInstance == null) {
            sInstance = new STProtoObject();
            sInstance.setClassProvider(new BindingClassProvider(sInstance) {
                @Override
                protected STClass _getSTClass() {
                    return null;
                }
            });
        }

        return sInstance;
    }
}
