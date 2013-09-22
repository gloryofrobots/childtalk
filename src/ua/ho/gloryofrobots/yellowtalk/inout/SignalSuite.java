package ua.ho.gloryofrobots.yellowtalk.inout;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.yellowtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;

public class SignalSuite {
    public static void error(String format, Object... args)
            throws RuntimeException {
        String error = String.format(format, args);
        InOutSuite.toStdErr(error);
        throw new RuntimeException(error);
    }

    public static void raiseError(Routine routine, String format,
            Object... args) {
        String txt = String.format(format, args);
        STString error = STString.create(txt);
        STExecutableObject object = routine.getExecutable();
        SchedulingSuite.callForSelectorWithArguments(routine, object,
                Universe.symbols().ERROR_COLON, error);
    }

    public static void warning(String format, Object... args)
            throws RuntimeException {
        String error = String.format(format, args);
        InOutSuite.toStdErr(error);
    }
}
