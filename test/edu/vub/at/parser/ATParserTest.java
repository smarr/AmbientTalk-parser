package edu.vub.at.parser;

import edu.vub.at.exceptions.InterpreterException;
import edu.vub.at.exceptions.XParseError;
import edu.vub.at.objects.natives.grammar.NATAbstractGrammar;
import edu.vub.at.parser.LexerImpl;
import edu.vub.at.parser.ParserImpl;
import edu.vub.at.parser.TreeWalkerImpl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import antlr.CommonAST;
import antlr.RecognitionException;
import antlr.TokenStreamException;
import junit.framework.TestCase;

public class ATParserTest extends TestCase {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(ATParserTest.class);
	}

	public static NATAbstractGrammar parseProgram(String parserInput) throws InterpreterException {
			try {
				InputStream input = new ByteArrayInputStream(parserInput.getBytes());
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
			InputStream input = new ByteArrayInputStream(parserInput.getBytes());
			LexerImpl lexer = new LexerImpl(input);
			ParserImpl parser = new ParserImpl(lexer);
			parser.program();

			CommonAST parseTree = (CommonAST)parser.getAST();
			System.out.println(parseTree.toStringList());
			assertEquals((expectedOutput).replaceAll("\\s", ""), parseTree.toStringList().replaceAll("\\s", ""));
		} catch(Exception e) {
			fail("Exception: "+e); 
		}
	}
	
	/**
	 * Tests for the validity of all statement abstract grammar elements.
	 * @covers all individual statement abstract grammar elements
	 */
	public void testStatementGrammar() {
		testParse("a;b;c",
				 "(begin (symbol a) (symbol b) (symbol c))");
		testParse("a;b;c;",
		          "(begin (symbol a) (symbol b) (symbol c))");
		testParse("def x := 5",
				 "(begin (define-field (symbol x) (number 5)))");
		testParse("def o.x := 5",
		          "(begin (define-external-field (symbol o) (symbol x) (number 5)))");
		testParse("def f(a,b) { 5 }",
				 "(begin (define-function (apply (symbol f) (table (symbol a) (symbol b))) (begin (number 5))))");
		testParse("def foo: x bar: y { 5 }",
				 "(begin (define-function (apply (symbol foo:bar:) (table (symbol x) (symbol y))) (begin (number 5))))");
		testParse("def o.f(a,b) { 5 }",
		          "(begin (define-external-method (symbol o) (apply (symbol f) (table (symbol a) (symbol b))) (begin (number 5))))");
		testParse("def o.foo: x bar: y { 5 }",
		          "(begin (define-external-method (symbol o) (apply (symbol foo:bar:) (table (symbol x) (symbol y))) (begin (number 5))))");		
		testParse("def t[5] { a }",
				 "(begin (define-table (symbol t) (number 5) (begin (symbol a))))");
		testParse("def t[5] { i := i + 1 }",
		          "(begin (define-table (symbol t) (number 5) (begin (var-set (symbol i) (+ (symbol i) (number 1))))))");
		testParse("def [x, y] := t",
                   "(begin (multi-def (table (symbol x) (symbol y)) (symbol t)))");
		testParse("x := 7",
				 "(begin (var-set (symbol x) (number 7)))");
		testParse("x[5] := 7",
		          "(begin (table-set (table-get ( symbol x ) ( number 5 ) ) (number 7)))");
		testParse("o.m := 1",
                   "(begin (field-set (select (symbol o) (symbol m)) (number 1)))");
		testParse("[x, y] := [ y, x]",
                  "(begin (multi-set (table (symbol x) (symbol y)) (table (symbol y) (symbol x))))");
		testParse("[x, y := 1] := a",
                  "(begin (multi-set (table (symbol x) (var-set (symbol y) (number 1))) (symbol a)))");
		testParse("defstripe foo",
				  "(begin (define-stripe (symbol foo) (table)))");
		testParse("defstripe foo <: bar",
				  "(begin (define-stripe (symbol foo) (table (symbol bar))))");
		testParse("defstripe foo <: bar, o.x",
		          "(begin (define-stripe (symbol foo) (table (symbol bar) (select (symbol o) (symbol x)))))");
	}
	
	/**
	 * Tests for the validity of all expression abstract grammar elements.
	 * @covers all individual expression abstract grammar elements
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
		          "(begin (select (symbol o) (symbol m)))");
		testParse(".m(a,b)",
				 "(begin (message (apply (symbol m) (table (symbol a) (symbol b)))))");
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
	}
	
	public void testQuasiquoting() {
		testParse("`(t[a] + 1)",
                  "(begin (quote (+ (table-get (symbol t) (symbol a)) (number 1))))");
         testParse("#(t[a] + 1)",
                    "(begin (unquote (+ (table-get (symbol t) (symbol a)) (number 1))))");
         testParse("#@(t[a] + 1)",
                   "(begin (unquote-splice (+ (table-get (symbol t) (symbol a)) (number 1))))");
 		testParse("`t[a] + 1",
 				  "(begin (+ (quote (table-get (symbol t) (symbol a))) (number 1)))");
 		testParse("#t[a] + 1",
 				  "(begin (+ (unquote (table-get (symbol t) (symbol a))) (number 1)))");
 		testParse("#@t[a] + 1",
 				  "(begin (+ (unquote-splice (table-get (symbol t) (symbol a))) (number 1)))");   
 		testParse("`{ def x := 5 }",
		          "(begin (quote (define-field (symbol x) (number 5))))");
 		testParse("`({ def x := 5 })",
                   "(begin (quote (closure (table) (begin (define-field (symbol x) (number 5))))))");
 		testParse("`foo:",
                  "(begin (quote (symbol foo:)))");
 		testParse("`foo:bar:",
                  "(begin (quote (symbol foo:bar:)))");
	}
	
	/**
	 * Tests for the validity of infix operator expressions and operator symbols in general
	 * @covers infix operators
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
	    testParse("+.m",
                  "(begin (select (symbol +) (symbol m)))");
	    testParse("-1",
                  "(begin (apply (symbol -) (table (number 1))))");
	    testParse("-t[5]",
                  "(begin (apply (symbol -) (table (table-get (symbol t) (number 5)))))");
	    testParse("-5 + a",
                  "(begin (+ (apply (symbol -) (table (number 5))) (symbol a)))");
	    testParse("/",
	              "(begin (symbol /))");
	    testParse("/.at",
	    		     "(begin (select (symbol /) (symbol at)))");
	    	testParse("~.test",
	    		     "(begin (select (symbol ~) (symbol test)))");
	}
	
	/**
	 * Tests syntax for literals
	 * @covers literal numbers, fractions, text, tables and closures
	 */
	public void testLiteralGrammar() {
		testParse("-12345",
				 "(begin (apply (symbol -) (table (number 12345))))");
		testParse("1.05",
		          "(begin (fraction 1.05))");
		testParse("-5.04e-10",
                   "(begin (apply (symbol -) (table (fraction 5.04e-10))))");
		testParse("\"hello  \\tworld\"",
				 "(begin (text \"hello  \\tworld\"))");
		testParse("[a,b,c]",
				 "(begin (table (symbol a) (symbol b) (symbol c)))");
		testParse("[]",
				 "(begin (table))");
		testParse("{ | x, y | x + y }",
				 "(begin (closure (table (symbol x) (symbol y)) (begin (+ (symbol x) (symbol y)))))");
		testParse("{ a := 2; b; }",
				 "(begin (closure (table) (begin (var-set (symbol a) (number 2)) (symbol b))))");
	}
	
	/**
	 * Tests syntax for variable arguments and splicing
	 * @covers variable arguments, parameters and spliced tables
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
	 * @covers selection invocation exp
	 * @covers canonical send invocation exp
	 * @covers keywordlist send invocation exp 
	 */
	public void testMessageSending() {
	    testParse(
	    		"object.no().demeter().law",
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
	}
	
	/**
	 * Tests grammar support for currying invocations - e.g. following references
	 * with an arbitrary amount of send expressions.
	 * @covers table
	 * @covers tabulation send exp
	 * @covers canonical application
	 */
	public void testCurrying() {
	    testParse(
	    		"[ { display: \"test\" }, { | x, y | x < y } ][2](a ,b)",
	    		" ( begin ( apply ( table-get ( table ( closure (table ) ( begin ( apply (symbol display:) (table (text \"test\" ) ) ) ) ) ( closure ( table (symbol x) (symbol y) ) ( begin ( < (symbol x) (symbol y) ) ) ) ) (number 2) ) ( table (symbol a) (symbol b) ) ) )");		
	
	    testParse("closures.at(closures.length)()",
	    		     "(begin (apply (send (symbol closures) (message (apply (symbol at) (table (select (symbol closures) (symbol length)))))) (table)))");
		testParse("closures[closures.length]()",
				 "(begin (apply (table-get (symbol closures) (select (symbol closures) (symbol length))) (table)))");
	}
	
	/**
	 * Test default behaviour for trailing keywords, and tests with correct nesting.
	 * @covers keywordlist application
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
	 * @covers keywordlist application
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
	 * Tests the definition of a prototype point object.
	 * @covers definition
	 * @covers assignment
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
	
}
