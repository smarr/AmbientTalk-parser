package edu.vub.at.parser;

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
		}
    }
	
	public void testParseErrors() {
		checkParseError("foo(1,2", "expecting a right parenthesis, found 'null'");
		checkParseError("def 1 := 5", "unexpected token: 1");
		checkParseError("5 := 2", "unexpected token: 5");
		checkParseError("f() g()", "unexpected token: g");
	}

}
