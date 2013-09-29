package ua.ho.gloryofrobots.childtalk.inout;

import ua.ho.gloryofrobots.childtalk.bootstrap.DebugSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STString;

public class SignalSuite {
    public static void error(String format, Object... args)
            throws RuntimeException {
        String error = String.format(format, args);
        if(DebugSuite.isDebugEnabled() == true) {
            InOutSuite.toStdErr(error);
        }
        
        throw new RuntimeException(error);
    }

    public static void raiseError(Routine routine, String format,
            Object... args) {
        String txt = String.format(format, args);
        STString error = STString.create(txt);
        STExecutableObject object = routine.getExecutable();
        SchedulingSuite.callForSelectorWithArguments(routine, object,
                ImageSuite.image().symbols().ERROR_COLON, error);
    }

    public static void warning(String format, Object... args)
            throws RuntimeException {
        String error = String.format(format, args);
        InOutSuite.toStdErr(error);
    }
}
