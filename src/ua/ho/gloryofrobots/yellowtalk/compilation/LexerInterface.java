package ua.ho.gloryofrobots.yellowtalk.compilation;


public interface LexerInterface {
	//TODO IMPORTANT NEGATIVE NUMBER IN LEXER!!!!!!!!!!!!!!!!!!!!!!!!!!
	public boolean lookup(int count) throws FileEvalException;
	public Token next() throws FileEvalException;
	public Token current();
	public Token previous();
	
	public void startTransaction();
	public void rollBackTransaction();
	public void endTransaction();
}