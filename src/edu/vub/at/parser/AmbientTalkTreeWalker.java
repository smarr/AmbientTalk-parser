/**
 * 
 */
package edu.vub.at.parser;

import antlr.ANTLRException;
import antlr.collections.AST;
import edu.vub.at.exceptions.InterpreterException;
import edu.vub.at.objects.natives.grammar.NATAbstractGrammar;

/**
 * Allowing for pluggable parser extensions, the {@link NATParser} class uses an abstract factory to create
 * both the Lexer, Parser and TreeWalker. In order to make the different implementations interchangable, they
 * must all implement a common interface. This interface details the functionality expected from a TreeWalker.
 * 
 * @author smostinc
 */
public abstract class AmbientTalkTreeWalker extends antlr.TreeParser {

	/**
	 * Transforms an ANTLR-specific abstract syntax tree to an AmbientTalk representation.
	 */
	public abstract NATAbstractGrammar	walkAST(AST tree) throws InterpreterException, ANTLRException;
}
