package ua.ho.gloryofrobots.yellowtalk;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.nio.CharBuffer;

public class ProgramTextStream implements ProgramTextStreamInterface {
    public class ProgramReadException extends Exception {
        public ProgramReadException(String string) {
            super(string);
        }

        private static final long serialVersionUID = 7279183773533242647L;

    }

    ProgramTextStream(InputStream stream) throws ProgramReadException {
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
        info.line = 0;
        info.column = 0;
        char ch;
        for (int i = 0; i < position; i++) {
            ch = mData[i];
            if (ch == '\n') {
                info.line++;
                info.column = 0;
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

}