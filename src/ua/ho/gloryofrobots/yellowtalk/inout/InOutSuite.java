package ua.ho.gloryofrobots.yellowtalk.inout;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;

public class InOutSuite {
    private static InputStream stdin;
    private static PrintStream stdout;
    private static PrintStream stderr;
    
    public static void init(InputStream in, PrintStream out, PrintStream err) {
        stdin = in;
        stdout = out;
        stderr = err;
    }
    
    public static void toStdOut(String text) {
        toOutputStream(stdout, text);
    }
    
    public static void toStdErr(String text) {
        toOutputStream(stderr, text);
    }

    private static void toOutputStream(PrintStream stream, String text) {
        stream.println(text);
    }
    
}
