package ua.ho.gloryofrobots.childtalk.compilation;

import java.io.InputStream;

public class ProgramTextStreamFile extends ProgramTextStream {

    private String mFileName;

    public ProgramTextStreamFile(InputStream stream, String fileName)
            throws ProgramReadException {
        super(stream);
        mFileName = fileName;
    }
    
    public String toString() {
        return mFileName;
    }
}
