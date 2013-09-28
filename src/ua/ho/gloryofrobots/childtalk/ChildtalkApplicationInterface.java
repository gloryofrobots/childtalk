package ua.ho.gloryofrobots.childtalk;

import java.io.InputStream;
import java.io.PrintStream;

public interface ChildtalkApplicationInterface {
    public InputStream getInputStream();
    public PrintStream getOutputStream();
    public PrintStream getErrorStream();
    
    public void onQuit();
}
