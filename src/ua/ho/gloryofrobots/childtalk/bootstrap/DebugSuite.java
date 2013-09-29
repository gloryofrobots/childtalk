package ua.ho.gloryofrobots.childtalk.bootstrap;

import ua.ho.gloryofrobots.childtalk.inout.InOutSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;

public class DebugSuite {
    private static int sDebugMode;
    private final static boolean sDebugEnable = false;
    public static final int DEBUG_MODE_LEXER = 1;
    public static final int DEBUG_MODE_PARSER = 2;
    public static final int DEBUG_MODE_COMPILER = 3;
    public static final int DEBUG_MODE_INTERPRETER = 4;
    public static final int DEBUG_MODE_BYTECODE = 5;
    public static final int DEBUG_MODE_SCHEDULER = 6;
    
    
    public static void setDebugMode(Integer... modes) {
        for(int mode : modes) {
            sDebugMode |= (1 << mode);
        }
    }
    
    public static final boolean isDebugEnabled() {
        return sDebugEnable;
    }
    
    public static boolean isOnDebugMode(int mode) {
        return (sDebugMode & (1 << mode)) != 0;
    }
    
    private static void _debugPrint(int mode, String message, Object... args) {
        if(isOnDebugMode(mode) == false) {
            return;
        }
        
        String format = String.format(message + InOutSuite.cr, args);
        InOutSuite.toStdOut(format);
    }
    
    public static void debugPrint(int mode, String message, Object... args) {
        //for jvm optimisation
        if(sDebugEnable) {
            _debugPrint(mode, message, args);
        }
    }
    
    public static String getTraceBackString(Routine routine) {
        
        StringBuilder builder = new StringBuilder();
        builder.append(routine.toString() + " called from : ");
        builder.append(InOutSuite.cr);
        Routine current = routine.getCaller();
        int count = 1;
        while(current != null) {
            count++;
            builder.append(current.toString());
            builder.append(InOutSuite.cr);
            current = current.getCaller();
        }
        
        builder.append("Trace length ");
        builder.append(count);
        return builder.toString();
    }
    
    public static void printTraceBackString(Routine routine) {
        InOutSuite.toStdErr(getTraceBackString(routine));
    }
}
