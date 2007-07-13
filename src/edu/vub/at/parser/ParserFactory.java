/**
 * 
 */
package edu.vub.at.parser;

import java.io.InputStream;

import antlr.CharScanner;

/**
 * Allowing for pluggable parsers is achieved by using the abstract factory described by this interface. 
 * 
 * @author smostinc
 */
public interface ParserFactory {

	public CharScanner createLexer(InputStream source);
	
	public AmbientTalkParser createParser(CharScanner lexer);
	
	public AmbientTalkTreeWalker createTreeWalker();
}
