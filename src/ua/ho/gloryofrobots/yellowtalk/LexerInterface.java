package ua.ho.gloryofrobots.yellowtalk;

public interface LexerInterface {
	//TODO IMPORTANT NEGATIVE NUMBER IN LEXER!!!!!!!!!!!!!!!!!!!!!!!!!!
	public boolean lookup(int count) throws FileEvalException;
	public Token next() throws FileEvalException;
	public Token current();
	public Token previous();
}