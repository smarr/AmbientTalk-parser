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
import edu.vub.at.exceptions.XParseError;
import edu.vub.at.objects.natives.grammar.NATAbstractGrammar;
import edu.vub.at.parser.LexerImpl;
import edu.vub.at.parser.ParserImpl;
import edu.vub.at.parser.TreeWalkerImpl;

import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.regex.Pattern;

import antlr.CharStreamException;
import antlr.CommonAST;
import antlr.RecognitionException;
import antlr.Token;
import antlr.TokenStreamException;
import junit.framework.TestCase;

public class ATParserTest extends TestCase {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(ATParserTest.class);
	}

	public static NATAbstractGrammar parseProgram(String parserInput) throws InterpreterException {
			try {
				Reader input = new StringReader(parserInput);
				LexerImpl lexer = new LexerImpl(input);
				ParserImpl parser = new ParserImpl(lexer);
				parser.program();

				CommonAST parseTree = (CommonAST)parser.getAST();
				TreeWalkerImpl walker = new TreeWalkerImpl();
				return walker.program(parseTree);
			} catch (RecognitionException e) {
				throw new XParseError(e.getMessage(), e);
			} catch (TokenStreamException e) {
				throw new XParseError(e.getMessage(), e);
			}
	}
	
	private static void testParse(String parserInput, String expectedOutput) {
		try {
			Reader input = new StringReader(parserInput);
			LexerImpl lexer = new LexerImpl(input);
			ParserImpl parser = new ParserImpl(lexer);
			parser.program();

			CommonAST parseTree = (CommonAST)parser.getAST();
			System.out.println(parseTree.toStringList());
			
            assertEquals(
            		Pattern.compile("\\s").matcher(expectedOutput).replaceAll(""),
            		Pattern.compile("\\s").matcher(parseTree.toStringList()).replaceAll(""));
		} catch(Exception e) {
			fail("Exception: "+e); 
		}
	}
	
	protected void showTokenStreamOf(String text) throws CharStreamException, TokenStreamException {
		Reader input = new StringReader(text);
		LexerImpl lexer = new LexerImpl(input);
		Token t = lexer.nextToken();
		while (t.getType() != Token.EOF_TYPE) {
			System.out.println(t.toString());
			t = lexer.nextToken();
		}
		System.out.println("<END OF STREAM>");
	}
	
	/**
	 * Tests for the validity of all statement abstract grammar elements.
	 * covers all individual statement abstract grammar elements
	 */
	public void testStatementGrammar() {
		testParse("", "(begin)");
		testParse("a;b;c",
				 "(begin (symbol a) (symbol b) (symbol c))");
		testParse("a;b;c;",
		          "(begin (symbol a) (symbol b) (symbol c))");
		testParse("def x",
		 		 "(begin (define-field (symbol x)))");
		testParse("def x := 5",
				 "(begin (define-field (symbol x) (number 5)))");
		testParse("def o.x",
				 "(begin (define-external-field (symbol o) (symbol x)))");
		testParse("def o.x := 5",
		          "(begin (define-external-field (symbol o) (symbol x) (number 5)))");
		testParse("def self.x := 5",
                  "(begin (define-external-field self (symbol x) (number 5)))");
		testParse("def f(a,b)",
				 "(begin (define-function (apply (symbol f) (table (symbol a) (symbol b)))))");
		testParse("def f(a,b) { 5 }",
				 "(begin (define-function (apply (symbol f) (table (symbol a) (symbol b))) (begin (number 5))))");
		testParse("def foo:=(a)",
		          "(begin (define-function (apply (symbol foo:=) (table (symbol a)))))");
		testParse("def foo: x bar: y",
				 "(begin (define-function (apply (symbol foo:bar:) (table (symbol x) (symbol y)))))");
		testParse("def foo: x bar: y { 5 }",
				 "(begin (define-function (apply (symbol foo:bar:) (table (symbol x) (symbol y))) (begin (number 5))))");
		testParse("def o.f(a,b)",
				 "(begin (define-external-method (symbol o) (apply (symbol f) (table (symbol a) (symbol b)))))");
		testParse("def self.f(a,b)",
		          "(begin (define-external-method self (apply (symbol f) (table (symbol a) (symbol b)))))");
		testParse("def o.f(a,b) { 5 }",
		          "(begin (define-external-method (symbol o) (apply (symbol f) (table (symbol a) (symbol b))) (begin (number 5))))");
		testParse("def o.foo: x bar: y",
				 "(begin (define-external-method (symbol o) (apply (symbol foo:bar:) (table (symbol x) (symbol y)))))");		
		testParse("def o.foo: x bar: y { 5 }",
		          "(begin (define-external-method (symbol o) (apply (symbol foo:bar:) (table (symbol x) (symbol y))) (begin (number 5))))");		
		testParse("def t[5]",
				 "(begin (define-table (symbol t) (number 5)))");
		testParse("def t[5] { a }",
				 "(begin (define-table (symbol t) (number 5) (begin (symbol a))))");
		testParse("def t[5] { i := i + 1 }",
		          "(begin (define-table (symbol t) (number 5) (begin (var-set (symbol i) (+ (symbol i) (number 1))))))");
		testParse("def [x, y]",
				 "(begin (multi-def (table (symbol x) (symbol y))))");
		testParse("def [x, y] := t",
                   "(begin (multi-def (table (symbol x) (symbol y)) (symbol t)))");
		testParse("x := 7",
				 "(begin (var-set (symbol x) (number 7)))");
		testParse("x[5] := 7",
		          "(begin (table-set (table-get ( symbol x ) ( number 5 ) ) (number 7)))");
		testParse("o.m := 1",
                   "(begin (field-set (send (symbol o) (field (symbol m))) (number 1)))");
		testParse("[x, y] := [ y, x]",
                  "(begin (multi-set (table (symbol x) (symbol y)) (table (symbol y) (symbol x))))");
		testParse("[x, y := 1] := a",
                  "(begin (multi-set (table (symbol x) (var-set (symbol y) (number 1))) (symbol a)))");
		testParse("deftype foo",
				  "(begin (define-type (symbol foo) (table)))");
		testParse("deftype foo <: bar",
				  "(begin (define-type (symbol foo) (table (symbol bar))))");
		testParse("deftype foo <: bar, x",
		          "(begin (define-type (symbol foo) (table (symbol bar) (symbol x))))");
		testParse("import o alias a := b, c := d exclude e, f",
				  "(begin (import (symbol o) (table (table (symbol a) (symbol b)) (table (symbol c) (symbol d))) (table (symbol e) (symbol f))))");
		testParse("import o.m()",
		          "(begin (import (send (symbol o) (message (apply (symbol m) (table)))) (table) (table)))");
		testParse("import o alias a: := b:c:",
		          "(begin (import (symbol o) (table (table (symbol a:) (symbol b:c:))) (table)))");
		testParse("import o exclude e, f:=",
		          "(begin (import (symbol o) (table) (table (symbol e) (symbol f:=))))");
		testParse("{}",
				  "(begin (closure (table) (begin)))");
		testParse("def m() {}",
		          "(begin (define-function (apply (symbol m) (table)) (begin)))");
	}
	
	/**
	 * Tests for the validity of all expression abstract grammar elements.
	 * covers all individual expression abstract grammar elements
	 */
	public void testExpressionGrammar() {
		testParse("o.m(a,b)",
				 "(begin (send (symbol o) (message (apply (symbol m) (table (symbol a) (symbol b))))))");
		testParse("o<-m(a,b)",
		          "(begin (send (symbol o) (async-message (apply (symbol m) (table (symbol a) (symbol b))))))");
		testParse("foo: a bar: b",
		          "(begin (apply (symbol foo:bar:) (table (symbol a) (symbol b))))");
		testParse("o.foo: a bar: b",
				 "(begin (send (symbol o) (message (apply (symbol foo:bar:) (table (symbol a) (symbol b))))))");
		testParse("o<-foo: a bar: b",
		          "(begin (send (symbol o) (async-message (apply (symbol foo:bar:) (table (symbol a) (symbol b))))))");
		testParse("super.m(a)",
				 "(begin (send (symbol super) (message (apply (symbol m) (table (symbol a))))))");
		testParse("m(a,b)",
				 "(begin (apply (symbol m) (table (symbol a) (symbol b))))");
		testParse("o.m",
		          "(begin (send (symbol o) (field (symbol m))))");
		testParse("o.&m",
                  "(begin (select (symbol o) (symbol m)))");
		testParse("o.&m:=",
                  "(begin (select (symbol o) (symbol m:=)))");
		testParse(".m(a,b)",
				 "(begin (message (apply (symbol m) (table (symbol a) (symbol b)))))");
		testParse(".x",
		          "(begin (field (symbol x)))");
		testParse("<-m(a,b)",
		          "(begin (async-message (apply (symbol m) (table (symbol a) (symbol b)))))");
		testParse("^m(a,b)",
                  "(begin (delegate (apply (symbol m) (table (symbol a) (symbol b)))))");
		testParse("t[a]",
		          "(begin (table-get (symbol t) (symbol a)))");
		testParse("f()[a+b]",
                  "(begin (table-get (apply (symbol f) (table)) (+ (symbol a) (symbol b))))");
		testParse("a",
                   "(begin (symbol a))");
		testParse("self",
                  "(begin self)");
		testParse("&x",
                  "(begin (lookup (symbol x)))");
		testParse("&x:=",
                  "(begin (lookup (symbol x:=)))");
		testParse("o.&foo:",
                  "(begin (select (symbol o) (symbol foo:)))");
		testParse("&foo:",
                  "(begin (lookup (symbol foo:)))");
	}
	
	public void testQuasiquoting() {
		testParse("`(t[a] + 1)",
                  "(begin (quote (+ (table-get (symbol t) (symbol a)) (number 1))))");
        testParse("#(t[a] + 1)",
                    "(begin (unquote (+ (table-get (symbol t) (symbol a)) (number 1))))");
        testParse("#@(t[a] + 1)",
                   "(begin (unquote-splice (+ (table-get (symbol t) (symbol a)) (number 1))))");
 		testParse("`t[a] + 1",
 				  "(begin (+ (table-get (quote (symbol t)) (symbol a)) (number 1)))");
 		testParse("`(t[a]) + 1",
		          "(begin (+ (quote (table-get (symbol t) (symbol a))) (number 1)))");
 		testParse("#t[a] + 1",
 				  "(begin (+ (table-get (unquote (symbol t)) (symbol a)) (number 1)))");
 		testParse("#(t[a]) + 1",
		          "(begin (+ (unquote (table-get (symbol t) (symbol a))) (number 1)))");
 		testParse("#@(t[a]) + 1",
 				  "(begin (+ (unquote-splice (table-get (symbol t) (symbol a))) (number 1)))");   
 		testParse("`{ def x := 5 }",
		          "(begin (quote-begin (begin (define-field (symbol x) (number 5)))))");
 		testParse("`({ def x := 5 })",
                   "(begin (quote (closure (table) (begin (define-field (symbol x) (number 5))))))");
 		testParse("`foo:",
                  "(begin (quote (symbol foo:)))");
 		testParse("`foo:bar:",
                  "(begin (quote (symbol foo:bar:)))");
 		testParse("`foo:=",
                  "(begin (quote (symbol foo:=)))");
 		testParse("`#name.m()",
                  "(begin (send (quote (unquote (symbol name))) (message (apply (symbol m) (table)))))");
 		testParse("`#(name.m())",
                  "(begin (quote (unquote (send (symbol name) (message (apply (symbol m) (table)))))))");
 		testParse("`{ def #(name) }",
				"(begin (quote-begin (begin (define-field (unquote-symbol (symbol name))))))");
 		testParse("`{ def #(name) := value }",
 				"(begin (quote-begin (begin (define-field (unquote-symbol (symbol name)) (symbol value)))))");
 		testParse("`{ def #(name) ( @args ); }",
 				"(begin (quote-begin (begin (define-function (apply (unquote-symbol (symbol name)) (table (splice (symbol args))))))))");
 		testParse("`{ def #(name) ( @args ) { system.println(args); }; }",
 				"(begin (quote-begin (begin (define-function (apply (unquote-symbol (symbol name)) (table (splice (symbol args)))) (begin (send (symbol system) (message (apply (symbol println) (table (symbol args))))))))))");
 		testParse("`{ def key: #(name) word: args; }",
 				"(begin (quote-begin (begin (define-function (apply (symbolkey:word:) (table (unquote (symbol name)) (symbol args)))))))");
 		testParse("`{ def #(name) [ 5 ] }",
 				"(begin (quote-begin (begin (define-table (unquote-symbol (symbol name)) (number 5)))))");
 		testParse("`{ def #(name) [ 5 ] { random(); } }",
 				"(begin (quote-begin (begin (define-table (unquote-symbol (symbol name)) (number 5) (begin (apply (symbol random) (table)))))))");
 		testParse("`{ def [ #(name), cancel ]; }",
 				"(begin (quote-begin (begin (multi-def (table (unquote (symbol name)) (symbol cancel))))))");
 		testParse("`{ def [ #(name), cancel ] := [ true, false ]; }",
 				"(begin (quote-begin (begin (multi-def (table (unquote (symbol name)) (symbol cancel)) (table (symbol true) (symbol false))))))");
 		testParse("`{ def #(object) . #(name) }",
 				"(begin (quote-begin (begin (define-external-field (unquote-symbol (symbol object)) (unquote-symbol (symbol name))))))");
 		testParse("`{ def #(object) . #(name) ( @args ); }",
 				"(begin (quote-begin (begin (define-external-method (unquote-symbol (symbol object)) (apply (unquote-symbol (symbol name)) (table (splice (symbol args))))))))");
 		testParse("`{ #(name) ( @args ); }",
 				"(begin (quote-begin (begin (apply (unquote (symbol name)) (table (splice (symbol args)))))))");
 		testParse("`{ #(receiver) .  #(name) ( @args ); }",
				"(begin (quote-begin (begin (send (unquote (symbol receiver)) (message (apply (unquote-symbol (symbol name))(table (splice (symbol args)))))))))");
 		testParse("`{ #(receiver) ^  #(name) ( @args ); }",
				"(begin (quote-begin (begin (send (unquote (symbol receiver)) (delegate (apply (unquote-symbol (symbol name)) (table (splice (symbol args)))))))))");
 		testParse("`{ #(receiver) <- #(name) ( @args ); }",
 				"(begin (quote-begin (begin (send (unquote (symbol receiver)) (async-message (apply (unquote-symbol (symbol name)) (table (splice (symbol args)))))))))");
 		testParse("`{ def foo(#@(`([a]))) { #@([1]) }}",
 				  "(begin (quote-begin (begin (define-function (apply (symbol foo) (table (unquote-splice (quote (table (symbol a)))))) (begin (unquote-splice (table (number 1))))))))");
 		testParse("import o alias #(foo) exclude #(bar)",
		          "(begin (import (symbol o) (unquote (symbol foo)) (unquote (symbol bar))))");
	}
	
	/**
	 * Tests for the validity of infix operator expressions and operator symbols in general
	 * covers infix operators
	 */
	public void testOperatorGrammar() {
		testParse("1 + 2 + 3",
				 "(begin (+ (+ (number 1) (number 2)) (number 3)))");
		testParse("a * b!3 + c < d / e - f",
		          "(begin (< (+ (* (symbol a) (! (symbol b) (number 3))) (symbol c)) (- (/ (symbol d) (symbol e)) (symbol f)))) ");
	    testParse("+(1,2)",
	    		     "(begin (apply (symbol +) (table (number 1) (number 2))))");
	    testParse("a.+(2)",
	              "(begin (send (symbol a) (message (apply (symbol +) (table (number 2))))))");
	    testParse("+.m(1)",
                  "(begin (send (symbol +) (message (apply (symbol m) (table (number 1))))))");
	    testParse("+.&m",
                  "(begin (select (symbol +) (symbol m)))");
	    testParse("m.&+",
                  "(begin (select (symbol m) (symbol +)))");
	    testParse("-1",
                  "(begin (apply (symbol -) (table (number 1))))");
	    testParse("-t[5]",
                  "(begin (apply (symbol -) (table (table-get (symbol t) (number 5)))))");
	    testParse("-5 + a",
                  "(begin (+ (apply (symbol -) (table (number 5))) (symbol a)))");
	    testParse("/",
	              "(begin (symbol /))");
	    testParse("/.at",
	    		     "(begin (send (symbol /) (field (symbol at))))");
	    	testParse("~.test",
	    		     "(begin (send (symbol ~) (field (symbol test))))");
	}
	
	/**
	 * Tests syntax for literals
	 * covers literal numbers, fractions, text, tables and closures
	 */
	public void testLiteralGrammar() {
		testParse("-12345",
				 "(begin (apply (symbol -) (table (number 12345))))");
		testParse("1.05",
		          "(begin (fraction 1.05))");
		testParse("-5.04e-10",
                   "(begin (apply (symbol -) (table (fraction 5.04e-10))))");
		testParse("\"hello  \\tworld\"",
				 "(begin (text \"hello  \tworld\"))");
		testParse("[a,b,c]",
				 "(begin (table (symbol a) (symbol b) (symbol c)))");
		testParse("[]",
				 "(begin (table))");
		testParse("{ | x, y | x + y }",
				 "(begin (closure (table (symbol x) (symbol y)) (begin (+ (symbol x) (symbol y)))))");
		testParse("{ a := 2; b; }",
				 "(begin (closure (table) (begin (var-set (symbol a) (number 2)) (symbol b))))");
		
		// Test unicode string literals as well
		testParse("\"a café at the VÜB\"",
		  "( begin ( text \"a café at the VÜB\" ))");
		testParse("\"unicode snowman for you – ☃\"",
		  "( begin ( text \"unicode snowman for you – ☃\" ))");
	}
	
	public void testInvalidEncodingInput() throws InterpreterException {
		String givenInput = "Negation_sign: ¬; Accented_e: é ";
		try {
			String brokenInput = new String(givenInput.getBytes("ISO-8859-1"), "UTF-8");
			NATParser.parse("testfile", brokenInput);
			fail("Parsing a string with invalid bytes should raise an error");
		} catch (UnsupportedEncodingException ignored) {
			/* won't happen */
		} catch (XParseError e) {
			assertEquals("The error should reference line 1, column 16", 1, e.getLine());
			assertEquals("The error should reference line 1, column 16", 16, e.getColumn());
		}
	}
	
	/**
	 * Tests syntax for variable arguments and splicing
	 * covers variable arguments, parameters and spliced tables
	 */
	public void testSplice() {
		testParse("def f(x,@y) { 1 }",
				 "(begin (define-function (apply (symbol f) (table (symbol x) (splice (symbol y)))) (begin (number 1))))");
		testParse("def foo: x bar: @y { 1 }",
		          "(begin (define-function (apply (symbol foo:bar:) (table (symbol x) (splice (symbol y)))) (begin (number 1))))");	
		testParse("def [x, @y] := t",
                   "(begin (multi-def (table (symbol x) (splice (symbol y))) (symbol t)))");
		testParse("f(1,@[2,3])",
                  "(begin (apply (symbol f) (table (number 1) (splice (table (number 2) (number 3))))))");
		testParse("foo: 1 bar: @[2,3]",
                   "(begin (apply (symbol foo:bar:) (table (number 1) (splice (table (number 2) (number 3))))))");
		testParse("[x, @[y,z], u, @v]",
                   "(begin (table (symbol x) (splice (table (symbol y) (symbol z))) (symbol u) (splice (symbol v))))");
		testParse("[x, @y] := [ u, v, w]",
                  "(begin (multi-set (table (symbol x) (splice (symbol y))) (table (symbol u) (symbol v) (symbol w))))");
	}

	/**
	 * Tests grammar support for message sends. 
	 * covers selection invocation exp
	 * covers canonical send invocation exp
	 * covers keywordlist send invocation exp 
	 */
	public void testMessageSending() {
	    testParse(
	    		"object.no().demeter().&law",
	    		" ( begin ( select ( send ( send (symbol object) (message ( apply (symbol no) (table))) ) (message ( apply (symbol demeter) (table) )) ) (symbol law) ) )");
	    testParse(
	    		"object.keyworded: message send: test",
	    		" ( begin ( send (symbol object) (message ( apply ( symbol keyworded:send:) (table (symbol message) (symbol test) ) ) ) ) )");
	    testParse(
	    		"o<+.m()",
	    		"(begin (send (symbol o) (univ-message (message (apply (symbol m) (table))))))");
	    testParse(
	    		"o<+foo()",
	    		"(begin (send (symbol o) (univ-message (apply (symbol foo) (table)))))");
	    testParse(
	    		"o^foo()",
	    		"(begin (send (symbol o) (delegate (apply (symbol foo) (table)))))");
	    testParse(
	    		"11.inc()",
	    		"(begin (send (number 11) (message (apply (symbol inc) (table)))))");
	}
	
	/**
	 * Tests grammar support for currying invocations - e.g. following references
	 * with an arbitrary amount of send expressions.
	 * covers table
	 * covers tabulation send exp
	 * covers canonical application
	 */
	public void testCurrying() {
	    testParse(
	    		"[ { display: \"test\" }, { | x, y | x < y } ][2](a ,b)",
	    		" ( begin ( apply ( table-get ( table ( closure (table ) ( begin ( apply (symbol display:) (table (text \"test\" ) ) ) ) ) ( closure ( table (symbol x) (symbol y) ) ( begin ( < (symbol x) (symbol y) ) ) ) ) (number 2) ) ( table (symbol a) (symbol b) ) ) )");		
	
	    testParse("closures.at(closures.length)()",
	    		     "(begin (apply (send (symbol closures)" +
	    		     "                    (message (apply (symbol at)" +
	    		     "                                    (table (send (symbol closures)" +
	    		     "                                                 (field (symbol length))))))) (table)))");
		testParse("closures[closures.length]()",
				 "(begin (apply (table-get (symbol closures) (send (symbol closures) (field (symbol length)))) (table)))");
	}
	
	/**
	 * Test default behaviour for trailing keywords, and tests with correct nesting.
	 */
	public void testTrailingKeywords() {
	    testParse(
	    		"if: c1 then: if: c2 then: a else: b", 
	    		" ( begin ( apply (symbol if:then:) (table (symbol c1) ( apply (symbol if:then:else:) (table (symbol c2) (symbol a) (symbol b) ) ) ) ) )");
	    testParse(
	    		"if: c1 then: ( if: c2 then: a ) else: b", 
	    		" ( begin ( apply (symbol if:then:else:) (table (symbol c1) ( apply (symbol if:then:) (table (symbol c2) (symbol a) ) ) (symbol b) ) ) )");
	}
	
	/**
	 * Test the syntax of optional arguments.
	 */
	public void testOptionalArgs() {
	    testParse("def foo(x := 5) { 1 }",
	    		"(begin (define-function (apply (symbol foo) (table (var-set (symbol x) (number 5)))) (begin (number 1))))");
	    testParse("def foo(x, y := 1+2, @z) { 1 }",
		          "(begin (define-function (apply (symbol foo) (table (symbol x) (var-set (symbol y) (+ (number 1) (number 2))) (splice (symbol z)))) (begin (number 1))))");
	    testParse("def foo(x := #5) { 1 }",
		          "(begin (define-function (apply (symbol foo) (table (var-set (symbol x) (unquote (number 5))))) (begin (number 1))))");
	}
	
	/**
	 * Test the syntax of annotations on message sends.
	 */
	public void testAnnotations() {
	    testParse("o.m(1)@x",
	    		  "(begin (send (symbol o) (message (apply (symbol m) (table (number1))) (symbol x))))");
	    testParse("o^m(1)@x",
		          "(begin (send (symbol o) (delegate (apply (symbol m) (table (number1))) (symbol x))))");
	    testParse("o<-m(1)@x",
		          "(begin (send (symbol o) (async-message (apply (symbol m) (table (number1))) (symbol x))))");
	    testParse("o.m(1)@[x,y]",
		          "(begin (send (symbol o) (message (apply (symbol m) (table (number1))) (table (symbol x) (symbol y)))))");
	}

	/**
	 * Tests the definition of a prototype point object.
	 * covers definition
	 * covers assignment
	 */
	public void testPointDefinition() {
		testParse(
				"def point := object: { | x, y | \n" +
				"  def getX() { x }; \n" +
				"  def getY() { y }; \n" +
				"  def withX: anX Y: aY { \n" +
				"    x := anX; \n" +
				"    y := anY \n" +
				"  } \n" +
				"} \n",
				"(begin" +
				  "(define-field (symbol point)" +
				                "(apply (symbol object:) (table (closure (table (symbolx) (symboly))" +
				                                               "(begin (define-function (apply (symbol getX) (table)) (begin (symbol x)))" +
				                                                      "(define-function (apply (symbol getY) (table)) (begin (symbol y)))" +
				                                                      "(define-function (apply (symbol withX:Y:) (table (symbol anX) (symbol aY)))" +
				                                                                      "(begin (var-set (symbol x) (symbol anX))" +
				                                                                             "(var-set (symbol y) (symbol anY))))))))))");
	}
	
	public void testComments() throws TokenStreamException, CharStreamException {
		testParse("/* test */ 1 /* the */ + /* multiline\n */ 2 /* comments */",
				  "(begin (+ (number 1) (number 2)))");
		testParse("{ nil /* an empty block */ }",
		          "(begin (closure (table) (begin (symbol nil))))");
		testParse("1 + 2//test single line comments",
		          "(begin (+ (number 1) (number 2)))");
		testParse("1 + 2//test single line comments ending in newline\n",
                  "(begin (+ (number 1) (number 2)))");
		// test whether operators that start with '/' work fine
		testParse("1 /+/ 2",
				  "(begin (/+/ (number 1) (number 2)))");
		// test whether '//' is not interpreted as an operator
		testParse("1 // 2\n",
				  "(begin (number 1))");
	}
	
	public void testKeywordedFunctions() throws TokenStreamException, CharStreamException {
		testParse("if: a then: b else: c",
		          "( begin ( apply ( symbol if:then:else: ) ( table ( symbol a ) ( symbol b ) ( symbol c ) ) ) )");
		testParse("if:then:else:(a,b,c)",
		          "( begin ( apply ( symbol if:then:else: ) ( table ( symbol a ) ( symbol b ) ( symbol c ) ) ) )");
	}
}
