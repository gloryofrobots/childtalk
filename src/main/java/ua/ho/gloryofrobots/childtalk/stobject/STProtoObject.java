package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

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
                    return ImageSuite.image().classes().Object;
                }
            });
        }

        return sInstance;
    }
}
