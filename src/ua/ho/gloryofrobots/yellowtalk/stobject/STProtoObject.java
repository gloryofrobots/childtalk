package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;

public class STProtoObject extends STMetaclass {
    private static final long serialVersionUID = 1L;
    private static STProtoObject sInstance;

    private STProtoObject() {
        super();
    }

    public static STProtoObject get() {
        if (sInstance == null) {
            sInstance = new STProtoObject();
        }

        return sInstance;
    }
}
