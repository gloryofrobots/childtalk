package ua.ho.gloryofrobots.childtalk.consoleapp;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import ua.ho.gloryofrobots.childtalk.ChildtalkPlatformInterface;

public class Platform implements ChildtalkPlatformInterface{
    @Override
    public void onQuit(int status) {
        System.exit(status);
    }

    @Override
    public PrintStream getOutputStream() {
        return System.out;
    }

    @Override
    public InputStream getInputStream() {
        return System.in;
    }

    @Override
    public PrintStream getErrorStream() {
        return System.err;
    }

    @Override
    public FileInputStream openFileInputStream(String path) throws FileNotFoundException{
        return new FileInputStream(path);
    }

    @Override
    public FileOutputStream openFileOutputStream(String path)
            throws FileNotFoundException {
        return new FileOutputStream(path);
    }
}
