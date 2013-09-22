package ua.ho.gloryofrobots.yellowtalk.bootstrap;

import ua.ho.gloryofrobots.yellowtalk.inout.InOutSuite;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.STObject;
import ua.ho.gloryofrobots.yellowtalk.stobject.STString;

public class DebugSuite {
    private static int sDebugMode;
    private final static boolean sDebugEnable = true;
    public static final int DEBUG_MODE_LEXER = 1;
    public static final int DEBUG_MODE_PARSER = 2;
    public static final int DEBUG_MODE_COMPILER = 3;
    public static final int DEBUG_MODE_INTERPRETER = 4;
    public static final int DEBUG_MODE_SCHEDULER = 4;
    
    public static void setDebugMode(Integer... modes) {
        for(int mode : modes) {
            sDebugMode |= (1 << mode);
        }
    }
    
    public static boolean isOnDebugMode(int mode) {
        return (sDebugMode & (1 << mode)) != 0;
    }
    
    private static void _debugPrint(int mode, String message, Object... args) {
        if(isOnDebugMode(mode) == false) {
            return;
        }
        System.out.printf(message + "\n", args);
    }
    
    public static void debugPrint(int mode, String message, Object... args) {
        //for jvm optimisation
        if(sDebugEnable) {
            _debugPrint(mode, message, args);
        }
    }
    
    public static String getTraceBackString(Routine routine) {
        StringBuilder builder = new StringBuilder();
        builder.append(routine.toString() + " called from : \n");
        Routine current = routine.getCaller();
        int count = 1;
        while(current != null) {
            count++;
            builder.append(current.toString() + "\n");
            current = current.getCaller();
        }
        
        builder.append("Trace length " + Integer.toString(count));
        return builder.toString();
    }
    
    public static void printTraceBackString(Routine routine) {
        SignalSuite.warning(getTraceBackString(routine));
    }
}
