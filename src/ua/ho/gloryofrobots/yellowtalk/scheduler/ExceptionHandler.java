package ua.ho.gloryofrobots.yellowtalk.scheduler;

import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;

public interface ExceptionHandler {
    public void onException(STObject exception, Routine routine); 
}
