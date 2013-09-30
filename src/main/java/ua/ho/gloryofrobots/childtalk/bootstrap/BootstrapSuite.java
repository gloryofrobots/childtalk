package ua.ho.gloryofrobots.childtalk.bootstrap;
import ua.ho.gloryofrobots.childtalk.ChildtalkPlatformInterface;
import ua.ho.gloryofrobots.childtalk.inout.InOutSuite;

public class BootstrapSuite {
    private static ChildtalkPlatformInterface sApplication;
    

    public static ChildtalkPlatformInterface application() {
        return sApplication;
    }

    public static boolean setApplication(
            ChildtalkPlatformInterface application) {
        sApplication = application;
        InOutSuite.init(application.getInputStream(),
                application.getOutputStream(), application.getErrorStream());
        return true;
    }
}
