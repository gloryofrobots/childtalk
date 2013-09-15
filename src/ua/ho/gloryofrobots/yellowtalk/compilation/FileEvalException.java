package ua.ho.gloryofrobots.yellowtalk.compilation;


public class FileEvalException extends Exception {
    private static final long serialVersionUID = -1075543908154251314L;

    FileEvalException(String message, LexerInterface lexer, int line, int column) {
	super(message);
	mLexer = lexer;
	mLine = line;
	mColumn = column;
    }

    public LexerInterface getLexer() {
	return mLexer;
    }

    public int getErrorLine() {
	return mLine;
    }

    public int getErrorColumn() {
	return mColumn;
    }

    protected LexerInterface mLexer;
    protected int mLine;
    protected int mColumn;
}
