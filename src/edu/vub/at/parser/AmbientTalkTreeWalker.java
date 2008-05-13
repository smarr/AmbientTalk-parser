/**
 * AmbientTalk/2 Project
 * Parser.java created on Aug 21, 2006 at 4:53:45 PM
 * (c) Programming Technology Lab, 2006 - 2007
 * Authors: Tom Van Cutsem & Stijn Mostinckx
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
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
