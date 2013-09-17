package ua.ho.gloryofrobots.yellowtalk.compilation;

import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigDecimal;

import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStream.ProgramReadException;
import ua.ho.gloryofrobots.yellowtalk.compilation.ProgramTextStreamInterface.PositionInfo;
import ua.ho.gloryofrobots.yellowtalk.compilation.Token.Type;
import ua.ho.gloryofrobots.yellowtalk.inout.SignalSuite;

public class Lexer{
    public class Transaction {
        private Lexer mLexer;
        private int mCount;

        Transaction(Lexer lexer) {
            mLexer = lexer;
            mCount = 0;
        }
        
        public void reset() {
            mCount = 0;
        }
        
        public void onNext() {
            mCount++;
        }
        
        public void onPrevious() {
            mCount--;
        }
        
        public void restore() {
            for(int i = 0; i < mCount; i++) {
                mLexer.previous();
            }
        }
    }
    
    Transaction mTransaction;
    private Lexer(ProgramTextStreamInterface programStream) throws FileEvalException {
        mProgramStream = programStream;
        mCurrentToken = new Token();
        mTransaction = new Transaction(this);
        next();
    }
    
    public static Lexer create(ProgramTextStreamInterface programStream)
            throws FileEvalException {
        Lexer lexer = new Lexer(programStream);
        return lexer;
    }
        
    public void lexerError(String txt, Token token) throws FileEvalException {
        try {
            
            ProgramTextStreamInterface.PositionInfo info = mProgramStream
                    .getPositionInfo(token.getPosition());
            String streamRepr = mProgramStream.toString();
            String streamPart = (streamRepr != null) ? (" in " + streamRepr) : "";
            
            String message = String.format("Syntax Error: %s%s at line %d column %d "
                    , txt, streamPart, info.line, info.column);
            throw new FileEvalException(message);
        } catch (IndexOutOfBoundsException e) {
            throwError(txt);
        }
    }

    private void throwError(String txt) {
        SignalSuite.error("Lexer error: %s",txt);
    }

    /*
     * private void lexerError(String txt) throws LexerException { throw new
     * LexerException(txt, this, -1, -1); }
     */

    public boolean lookup(int count) throws FileEvalException {
        if (mCurrentToken.type == Token.Type.END) {
            return false;
        }

        Token current = mCurrentToken;
        for (int i = 0; i < count; ++i) {
            _next();
            if (mCurrentToken.type == Token.Type.END) {
                return false;
            }
        }
        // Restore current token
        mCurrentToken = current;
        return true;
    }

    public Token next() throws FileEvalException {
        _next();
        mTransaction.onNext();
        return mCurrentToken;
        
    }
    
    public void _next() throws FileEvalException {
        if (mCurrentToken.type == Token.Type.END) {
            return;
        }

        if (mCurrentToken.getNext() == null) {
            Token token = doNextToken();
            mCurrentToken.setNext(token);
            token.setPrevious(mCurrentToken);
            mCurrentToken = token;

        } else {
            mCurrentToken = mCurrentToken.getNext();
        }
    }
    
    
    public Token current() {
        return mCurrentToken;
    }

    
    public Token previous() {
        if (mCurrentToken.getPrevious() == null) {
            return null;
        }

        mCurrentToken = mCurrentToken.getPrevious();
        mTransaction.onPrevious();
        return mCurrentToken;
    }

    Token mCurrentToken;
    ProgramTextStreamInterface mProgramStream;

    private char forwardChar() {
        if (mProgramStream.goForward() == false) {
            return '\0';
        } else {
            return mProgramStream.getCurrentChar();
        }
    }

    private void pushBackChar(int count) throws FileEvalException {
        if (mProgramStream.goBackward(count) == false) {
            throwError(String.format("INVALID Backward roll value %d", count));
        }
    }

    private boolean isSpace(char ch) {
        return Character.isWhitespace(ch);
    }

    private boolean isAlpha(char ch) {
        return Character.isLetter(ch);
    }

    private boolean isDigit(char ch) {
        return Character.isDigit(ch);
    }

    private boolean isAlphaOrDigit(char ch) {
        return isAlpha(ch) || isDigit(ch);
    }

    private char toLowerChar(char ch) {
        return Character.toLowerCase(ch);
    }

    private boolean isXDigit(char ch) {
        return !(Character.digit(ch, 16) == -1);
    }

    private Token doNextToken() throws FileEvalException {

        char lastChar;
        lastChar = skipComment();
        Token token = new Token();
        int position = mProgramStream.getPosition();
        token.setPosition(position);

        if (lastChar == '\0') {
            token.type = Token.Type.END;
        }

        else if (isAlpha(lastChar)) {
            matchIdentifier(token, lastChar);
        }
        
        else if (lastChar == '-') {
            if (isDigit(forwardChar())) {
                pushBackChar(1);
                matchNumber(token, lastChar);
            }
            else {
                pushBackChar(1);
                matchSecondBinary(token, lastChar);
            }
        }
        
        else if (isDigit(lastChar)) {
            matchNumber(token, lastChar);
        }

        else if (lastChar == '$') {
            matchCharacter(token, lastChar);
        }

        else if (lastChar == '#') {
            if (forwardChar() == '(')
                token.type = Token.Type.ARRAY_BEGIN;
            else {
                pushBackChar(1);
                matchSymbol(token, lastChar);
            }
        }

        else if (lastChar == '\'') {
            matchString(token, lastChar);
        }

        else if (isClosing(lastChar)) {
            matchClosing(token, lastChar);
        }
        else if (isSingleBinary(lastChar)) {
            matchBinary(token, lastChar);
        }
        

        else {
            matchSecondBinary(token, lastChar);
        }

        return token;
    }

    private char skipComment() {
        char lastChar;
        do {
            lastChar = forwardChar();
            if (lastChar == '"') {
                lastChar = forwardChar();
                while (lastChar != '\0' && lastChar != '"')
                    lastChar = forwardChar();
            }
        } while (lastChar != '\0' && (isSpace(lastChar) || lastChar == '"'));

        return lastChar;
    }

    private void matchSecondBinary(Token token, char lastChar)
            throws FileEvalException {
        String str = new String();
        str += lastChar;
        lastChar = forwardChar();
        if (isSecondBinary(lastChar)) {
            str += lastChar;
        } else {
            pushBackChar(1);
        }

        token.evalSpecial(str, Token.Type.BINARY);
    }

    private boolean isSecondBinary(char ch) {
        return !(isAlphaOrDigit(ch) || isSpace(ch) || isClosing(ch) || isSingleBinary(ch));
    }

    private void matchBinary(Token token, char lastChar) {
        token.evalCharacter(lastChar, Token.Type.BINARY);
    }

    private boolean isSingleBinary(char ch) {
        switch (ch) {
        case '{':
        case '[':
        case '(':
        case ')':
        case ']':
        case '}':
        case '!':
        case '^':
            return true;
        }
        return false;
    }

    private void matchClosing(Token token, char lastChar) {
        token.evalCharacter(lastChar, Token.Type.CLOSING);
    }

    private boolean isClosing(char ch) {
        switch (ch) {
        case '.':
        case ']':
        case ')':
        case '}':
        case ';':
        case '"':
        case '\'':
            return true;
        }
        return false;
    }

    private String getStringFromStream() throws FileEvalException {
        char lastChar;
        String str = new String();

        while (true) {

            while ((lastChar = forwardChar()) != '\0' && lastChar != '\'') {
                str += lastChar;
            }

            lastChar = forwardChar();

            if (lastChar == '\'') {
                str += lastChar;
            } else {
                pushBackChar(1);
                break;
            }
        }
        return str;
    }

    private void matchString(Token token, char lastChar)
            throws FileEvalException {
        String str = getStringFromStream();
        token.evalString(str);
    }

    private void matchSymbol(Token token, char lastChar)
            throws FileEvalException {

        String str = new String();
        /* check for symbol enclosed by quotes like a string #'symbol' */
        lastChar = forwardChar();
        if (lastChar == '\'') {
            str = getStringFromStream();
            token.evalSpecial(str, Token.Type.SYMBOL_CONST);
            return;
        }

        /*
         * if it's not an alpha numeric symbol, be sure to return a symbol of
         * length 2 representing a binary symbol
         */
        if (lastChar == '-' || isSecondBinary(lastChar)) {
            str += lastChar;
            lastChar = forwardChar();

            if (lastChar == '-' || isSecondBinary(lastChar)) {
                str += lastChar;

            } else {
                pushBackChar(1);
            }
        } else {
            while (lastChar != '\0'
                    && ((isAlphaOrDigit(lastChar) || lastChar == ':'))) {
                str += lastChar;

                lastChar = forwardChar();
            }
            pushBackChar(1);
        }

        token.evalSpecial(str, Token.Type.SYMBOL_CONST);

    }

    private void matchCharacter(Token token, char lastChar) {
        lastChar = forwardChar();
        token.evalCharacter(lastChar, Token.Type.CHAR_CONST);
    }

    private void matchNumber(Token token, char lastChar)
            throws FileEvalException {
        String str = new String();

        int radix = 10;
        boolean sign = false;

        do {
            str += lastChar;
            lastChar = forwardChar();
        } while (lastChar != '\0' && isDigit(lastChar));
        
        if (token.evalInteger(str, radix) == false) {
            if (token.evalLargeInteger(str, radix) == false) {
                lexerError("Error in integer format", token);
            }
        }

        /* a radix? */
        if (toLowerChar(lastChar) == 'r') {
            radix = token.integerValue();
            if (radix < 2 || radix > 36) {
                lexerError("Invalid integer radix", token);
            }

            str = "";
            while ((lastChar = forwardChar()) != '\0' && isXDigit(lastChar)) {
                str += lastChar;
            }

            if (str.length() == 0) {
                pushBackChar(1);
                return;
            }

            token.clear();
           
            if (token.evalInteger(str, radix) == false) {
                if (token.evalLargeInteger(str, radix) == false) {
                    lexerError("Error in integer format after radix", token);
                }
            }

        }
        /* a float? */
        if (lastChar == '.') {
            if ((lastChar = forwardChar()) != '\0' && isDigit(lastChar)) {
                str += '.';
                do {
                    str += lastChar;
                    lastChar = forwardChar();
                } while (lastChar != '\0' && isDigit(lastChar));

                token.clear();

                if (token.evalFloat(str) == false) {
                    lexerError("Error in float format", token);
                }
            } else {
                pushBackChar(2);
                return;
            }
        }

        /* float e? */
        if (lastChar == 'e') {
            if ((lastChar = forwardChar()) == '-') {
                sign = true;
                lastChar = forwardChar();
            }

            if (lastChar != '\0' && isDigit(lastChar)) {
                str += 'e';
                if (sign)
                    str += '-';
                do {
                    str += lastChar;
                    lastChar = forwardChar();
                } while (lastChar != '\0' && isDigit(lastChar));

                pushBackChar(1);

                if (token.evalFloat(str) == false) {
                    lexerError("Error in float format", token);
                }

            } else {
                if (sign) {
                    pushBackChar(3);
                } else {
                    pushBackChar(2);
                }

            }
        }
        else {
            pushBackChar(1);
        }
    }

    private void matchIdentifier(Token token, char lastChar)
            throws FileEvalException {
        String str = new String();
        Token.Type type = null;
        str += lastChar;

        while ((lastChar = forwardChar()) != '\0' && isAlphaOrDigit(lastChar)) {
            str += lastChar;
        }

        if (lastChar == ':') {
            lastChar = forwardChar();
            /* This one is an assignment := */
            if (lastChar == '=') {
                pushBackChar(2);
                type = Token.Type.NAME_CONST;
            } else {
                pushBackChar(1);
                str += ':';
                type = Token.Type.NAME_COLON;
            }
        } else {
            pushBackChar(1);
            type = Token.Type.NAME_CONST;
        }

        token.evalSpecial(str, type);
    }

    public static void main(String[] args) {
        FileInputStream fileStream = null;
        try {
            fileStream = new FileInputStream(
                    "/home/gloryofrobots/develop/smalltalk/yellowtalk/st/tests/lexer_test.st");
            ProgramTextStreamInterface programStream = new ProgramTextStream(
                    fileStream);
            Lexer lexer = Lexer.create(programStream);
            Token token;
            while(true) {
                token = lexer.current();
                if(token.type == Token.Type.END) {
                    break;
                }
                lexer.next();
                System.out.printf("Type : %s Value : '%s' \n", token.type.toString(), token.stringValue());
            }
        } catch (IOException x) {
            System.err.println(x);
        } catch (ProgramReadException e) {
            System.err.println(e);
        } catch (FileEvalException e) {
            e.printStackTrace();
        } finally {
            if (fileStream != null) {
                try {
                    fileStream.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    
    public void startTransaction() {
        mTransaction.reset();
    }

    
    public void rollBackTransaction() {
        mTransaction.restore();
        endTransaction();
    }

    
    public void endTransaction() {
        mTransaction.reset();
    }

    public int getCurrentTokenPosition() {
        return current().getPosition();
    }

    public int getPreviousTokenPosition() {
        return current().getPrevious().getPosition();
    }

    public ProgramTextStreamInterface getStream() {
        return mProgramStream;
    }
}
