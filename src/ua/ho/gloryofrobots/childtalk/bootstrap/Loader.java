package ua.ho.gloryofrobots.childtalk.bootstrap;

import java.io.FileInputStream;
import java.io.IOException;

import ua.ho.gloryofrobots.childtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStream;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStreamFile;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStreamInterface;
import ua.ho.gloryofrobots.childtalk.compilation.Compiler.UnsupportedNodeException;
import ua.ho.gloryofrobots.childtalk.compilation.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STImage;

public class Loader {
    
    public static ProgramTextStreamInterface createProgramStream(String path) {
        FileInputStream fileStream = null;
        ProgramTextStreamInterface programStream = null;
        try {
            fileStream = new FileInputStream(path);
            programStream = new ProgramTextStreamFile(fileStream,path);
        } catch (IOException x) {
            SignalSuite.error("IO error in file %s - %s", path, x.getMessage());
        } catch (ProgramReadException e) {
            SignalSuite.error("Program code reading error  %s - %s", path, e.getMessage());
        } finally {
            if (fileStream != null) {
                try {
                    fileStream.close();
                } catch (IOException e) {
                    SignalSuite.error("IO error closing file %s - %s", path, e.getMessage());
                }
            }
        }
        
        return programStream;
    }
    
    public void loadAndCompileFile(String path, STImage image) {
        ProgramTextStreamInterface programStream = createProgramStream(path);
        if(programStream == null) {
            //SignalSuite.warning("error loading file %s ", path);
            return;
        }
        CompileSuite.compileProgramStream(programStream, image);
    }

    public void loadAndCompileClass(String folderPath, String className, STImage image) {
        String classPath = createClassPath(folderPath, className);
        loadAndCompileFile(classPath, image);
    }

    private String createClassPath(String folder, String className) {
        return String.format("%s/%s", folder, className);
    }

    public void loadAndCompileClassesFromFolder(String folderPath, String[] classNames,
            STImage image) {
        for (String className : classNames) {
            loadAndCompileClass(folderPath, className, image);
        }
    }

}
