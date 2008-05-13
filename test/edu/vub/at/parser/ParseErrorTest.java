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

import edu.vub.at.exceptions.InterpreterException;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.TestCase;
import antlr.CommonAST;
import antlr.RecognitionException;
import antlr.TokenStreamException;

public class ParseErrorTest extends TestCase {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(ParseErrorTest.class);
	}
	
	public static void checkParseError(String parserInput, String errmsg) {
		try {
			InputStream input = new ByteArrayInputStream(parserInput.getBytes());
			LexerImpl lexer = new LexerImpl(input);
			ParserImpl parser = new ParserImpl(lexer);
			parser.program();

			CommonAST parseTree = (CommonAST)parser.getAST();
			TreeWalkerImpl walker = new TreeWalkerImpl();
			walker.program(parseTree);
			fail("No parse error in " + parserInput + ", expected " + errmsg);
		} catch (RecognitionException e) {
			assertEquals(errmsg, e.getMessage());
		} catch (TokenStreamException e) {
			assertEquals(errmsg, e.getMessage());
		} catch (InterpreterException e) {
			assertEquals(errmsg, e.getMessage());
		}
    }
	
	public void testParseErrors() {
		checkParseError("foo(1,2", "expecting a right parenthesis, found 'null'");
		checkParseError("def 1 := 5", "unexpected token: 1");
		checkParseError("5 := 2", "unexpected token: 5");
		checkParseError("f() g()", "unexpected token: g");
		checkParseError("[x := 1,a] := [1,2]", "Illegal parameter list for multi-assignment: optional parameters followed by mandatory parameter a");
	}

}
