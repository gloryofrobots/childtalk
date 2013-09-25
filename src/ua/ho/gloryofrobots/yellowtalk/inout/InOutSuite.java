package ua.ho.gloryofrobots.yellowtalk.inout;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;

import ua.ho.gloryofrobots.yellowtalk.stobject.STByteObject;

public class InOutSuite {
    private static InputStream stdin;
    private static PrintStream stdout;
    private static PrintStream stderr;
    public static final String cr;
    
    static{
        cr = System.getProperty("line.separator");
    }
    
    public static void init(InputStream in, PrintStream out, PrintStream err) {
        stdin = in;
        stdout = out;
        stderr = err;
    }
    
    public static void toStdOut(String text) {
        toOutputStream(stdout, text);
    }
    
    public static void toStdOut(STByteObject obj) {
        toOutputStream(stdout, obj);
    }
    
    public static void toStdErr(String text) {
        toOutputStream(stderr, text);
    }

    private static void toOutputStream(PrintStream stream, String text) {
        stream.println(text);
    }
    
    private static void toOutputStream(PrintStream stream, STByteObject obj) {
        char [] data = obj.toCharArray();
        for(char ch : data) {
            if(ch == 0) {
                break;
            }
            stream.print(ch);
        }
        
    }
    
}
