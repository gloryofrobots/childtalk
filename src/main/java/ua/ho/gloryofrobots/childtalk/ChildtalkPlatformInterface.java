package ua.ho.gloryofrobots.childtalk;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

public interface ChildtalkPlatformInterface {
    public InputStream getInputStream();
    public PrintStream getOutputStream();
    public PrintStream getErrorStream();
    
    public FileInputStream openFileInputStream(String path)  throws FileNotFoundException;
    public FileOutputStream openFileOutputStream(String path)  throws FileNotFoundException;
    public void onQuit();
}
