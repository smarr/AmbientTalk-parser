/**
 * 
 */
package edu.vub.at.parser;

import antlr.ANTLRException;
import antlr.collections.AST;

/**
 * Allowing for pluggable parser extensions, the {@link NATParser} class uses an abstract factory to create
 * both the Lexer, Parser and TreeWalker. In order to make the different implementations interchangable, they
 * must all implement a common interface. This interface details the functionality expected from a Parser.
 * 
 * @author smostinc
 */
public interface AmbientTalkParser {

	/**
	 * Parse a program according to the grammar rules specified and return the resulting abstract syntax tree.
	 */
	public abstract AST	parseProgram() throws ANTLRException;
	
	/**
	 * Sets the name of the file which is currently being parse for proper error reporting
	 */
	public abstract void setFilename(String name);
}

