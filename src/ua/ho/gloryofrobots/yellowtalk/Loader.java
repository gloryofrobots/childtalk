package ua.ho.gloryofrobots.yellowtalk;

import java.io.FileInputStream;
import java.io.IOException;

import ua.ho.gloryofrobots.yellowtalk.Compiler.UnsupportedNodeException;
import ua.ho.gloryofrobots.yellowtalk.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.yellowtalk.stobject.STImage;

public class Loader {
    Compiler mCompiler;
    
    public Loader() {
        mCompiler = new Compiler();
    }
    
    public void loadFile(String path, STImage image) {
        FileInputStream fileStream = null;
        try {
            fileStream = new FileInputStream(path);
            ProgramTextStreamInterface programStream = new ProgramTextStream(fileStream);
            LexerInterface lexer = new Lexer(programStream);
            
            mCompiler.compile(lexer, image);
        } catch (IOException x) {
            System.err.println(x);
        } catch (ProgramReadException e) {
            System.err.println(e);
        } catch (FileEvalException e) {
            e.printStackTrace();
        } catch(UnsupportedNodeException e) {
            e.printStackTrace();
        }
        finally {
            if (fileStream != null) {
                try {
                    fileStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
    
    public void loadClass(String folderPath, String className, STImage image) {
        String classPath = createClassPath(folderPath, className);
        loadFile(classPath, image);
    }
    
    public String createClassPath(String folder, String className) {
        return String.format("%s/%s.st", folder, className);
    }
    
    public void loadClassesFromFolder(String folderPath, String[] classNames, STImage image) {
        for(String className : classNames) {
            loadClass(folderPath, className, image);
        }
    }
}
