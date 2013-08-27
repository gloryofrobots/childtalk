package ua.ho.gloryofrobots.yellowtalk;

import ua.ho.gloryofrobots.yellowtalk.node.Node;

public interface ParserInterface {
	public Node parse() throws FileEvalException;
	public void setLexer(LexerInterface lexer);
}
		
