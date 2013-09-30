package ua.ho.gloryofrobots.childtalk.compilation;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;


public class ProgramTextStream implements ProgramTextStreamInterface {
    public class ProgramReadException extends Exception {
        
        private static final long serialVersionUID = 1L;

        public ProgramReadException(String string) {
            super(string);
        }
    }

    public ProgramTextStream(InputStream stream) throws ProgramReadException {
        readData(stream);
        mCurrentIndex = 0;
        mCurrentChar = 0;
    }

    private void readData(InputStream stream) throws ProgramReadException {
        final char[] buffer = new char[200];
        final StringBuilder out = new StringBuilder();

        try {
            final Reader reader = new InputStreamReader(stream, "UTF-8");
            try {
                while (true) {
                    int size = reader.read(buffer, 0, buffer.length);
                    if (size < 0) {
                        break;
                    }

                    out.append(buffer, 0, size);
                }
            } finally {
                reader.close();
            }
        } catch (UnsupportedEncodingException ex) {
            throw new ProgramReadException("Unsapported encoding");
        } catch (IOException ex) {
            throw new ProgramReadException("IO Error");
        }

        mData = out.toString().toCharArray();
    }

    public boolean isEmpty() {
        return mData.length == 0;
    }

    @Override
    public boolean goForward() {
        if (mData.length == 0) {
            return false;
        }

        if (mCurrentIndex >= mData.length) {
            return false;
        }

        mCurrentChar = mData[mCurrentIndex];

        mCurrentIndex++;
        return true;
    }

    @Override
    public boolean goBackward(int count) {
        int newIndex = (mCurrentIndex - count);
        if (newIndex < 0) {
            return false;
        }

        mCurrentIndex = newIndex;
        return true;
    }

    @Override
    public char getCurrentChar() {
        return mCurrentChar;
    }

    @Override
    public PositionInfo getPositionInfo(int position)
            throws IndexOutOfBoundsException {

        if (position < 0 || position > mData.length) {
            throw new IndexOutOfBoundsException(
                    "invalid position for determining line and column"
                            + String.valueOf(position));
        }

        PositionInfo info = new PositionInfo();
        info.line = 1;
        info.column = 1;
        char ch;
        for (int i = 0; i < position; i++) {
            ch = mData[i];
            if (ch == '\n') {
                info.line++;
                info.column = 1;
            } else {
                info.column++;
            }
        }
        
        return info;
    }

    @Override
    public int getPosition() {
        return mCurrentIndex - 1;
    }

    char[] mData;
    char mCurrentChar;
    int mCurrentIndex;
    
    @Override
    public boolean isEnd() {
        return mCurrentIndex >= mData.length;
    }

    @Override
    public char[] getRange(int first, int last) {
        int size = last - first;
        if(size < 0) {
            return null;
        }
        
        char[] copy = new char[size];
        System.arraycopy(mData, first, copy, 0, size);
        return copy;
    }
    
    public String toString() {
        return null;
    }
}