package ua.ho.gloryofrobots.yellowtalk.inout;

public class SignalSuite {
    public static void error(String format, Object... args) throws RuntimeException {
        String error = String.format(format, args);
        InOutSuite.toStdErr(error);
        throw new RuntimeException(error);
    }
}
