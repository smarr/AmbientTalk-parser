package edu.vub.at.parser;

import edu.vub.at.objects.natives.grammar.NATAbstractGrammar;
import edu.vub.at.parser.LexerImpl;
import edu.vub.at.parser.ParserImpl;
import edu.vub.at.parser.TreeWalkerImpl;

import java.io.ByteArrayInputStream;

import junit.framework.TestCase;
import antlr.CommonAST;

public class ATWalkerTest extends TestCase {
	
	public static void main(String[] args) {
		junit.swingui.TestRunner.run(ATWalkerTest.class);
	}
	
	private void testWalker(String walkerInput) {
       testWalker(walkerInput, walkerInput);
	}
	
	private void testWalker(String input, String output) {
        try {
            LexerImpl lexer = new LexerImpl(new ByteArrayInputStream(input.getBytes()));
            ParserImpl parser = new ParserImpl(lexer);
            // Parse the input expression
            parser.program();
            CommonAST t = (CommonAST)parser.getAST();
            // Print the resulting tree out in LISP notation
            System.out.println(t.toStringList());
            TreeWalkerImpl walker = new TreeWalkerImpl();
            // Traverse the tree created by the parser
            NATAbstractGrammar ag = walker.program(t);
            assertEquals(output.replaceAll("\\s",""), ag.meta_print().javaValue.replaceAll("\\s",""));
        } catch(Exception e) {
        		e.printStackTrace();
            fail("exception: "+e);
        }
	}
	
	public void testStatementGrammar() {
		testWalker("a; b; c");
		testWalker("def x", "def x := nil");
		testWalker("def x := 5");
		testWalker("def o.x", "def o.x := nil");
		testWalker("def o.x := 5");
		testWalker("def f(a,b)", "def f(a,b) { nil }");
		testWalker("def f(a,b) { 5 }");
		testWalker("def foo: x bar: y", "def foo:bar:(x,y) { nil }");
		testWalker("def foo: x bar: y { 5 }", "def foo:bar:(x,y) { 5 }");
		testWalker("def o.f(a,b)", "def o.f(a,b) { nil }");
		testWalker("def o.f(a,b) { 5 }");
		testWalker("def o.foo: x bar: y", "def o.foo:bar:(x,y) { nil }");
		testWalker("def o.foo: x bar: y { 5 }", "def o.foo:bar:(x,y) { 5 }");
		testWalker("def t[5]", "def t[5] { nil }");
		testWalker("def t[5] { a }");
		testWalker("def t[5] { i := i + 1 }", "def t[5] { i := i.+(1) }");
		testWalker("def [x, y]", "def [x, y] := [ nil, nil ]");
		testWalker("def [x, y] := [y, x]");
		testWalker("x := 7");
		testWalker("x[5] := 7");
		testWalker("o.m := x");
		testWalker("[x, y] := [y, x]");
		testWalker("[x, y := 1] := a");
		testWalker("defstripe foo");
		testWalker("defstripe foo <: bar");
		testWalker("defstripe foo <: bar, o.x");
	}
	
	public void testExpressionGrammar() {
		testWalker("o.m(a,b)");
		testWalker("o<-m(a,b)");
		testWalker("o.foo: a bar: b", "o.foo:bar:(a,b)");
		testWalker("o<-foo: a bar: b", "o<-foo:bar:(a,b)");
		testWalker("super.m(a)");
		testWalker("m(a,b)");
		testWalker("f()");
		testWalker("o.m");
		testWalker(".m(a,b)");
		testWalker("<-m(a,b)");
		testWalker("^m(a,b)");
		testWalker("t[a]");
		testWalker("f()[a+b]", "f()[a.+(b)]");
		testWalker("a");
	    testWalker("o<+.m()","o.m()");
	    testWalker("o<+foo()","o<+foo()");
	    testWalker("o<+^foo()","o^foo()");
	    testWalker("obj^m(a)");
	}
	
	public void testQuasiquoting() {
		testWalker("`(x)");
		testWalker("`(t[a].+(1))");
		testWalker("#(t[a].+(1))");
		testWalker("#@(t[a].+(1))");
		testWalker("`t[a] + 1", "`(t[a]).+(1)");
		testWalker("#t[a] + 1", "#(t[a]).+(1)");
		testWalker("#@t[a] + 1", "#@(t[a]).+(1)");
		testWalker("`{ def x := 5 }", "`( def x := 5 )");
		testWalker("`({ def x := 5 })");
		testWalker("`foo:","`(foo:)");
		testWalker("`foo:bar:","`(foo:bar:)");
	}
	
	public void testOperatorGrammar() {
		testWalker("1 + 2 + 3", "1.+(2).+(3)");
		testWalker("a * b!3 + c < d / e - f", "a.*(b.!(3)).+(c).<(d./(e).-(f))");
		testWalker("+(1,2)");
		testWalker("a.+(2)");
		testWalker("+.m(1)");
		testWalker("+.m");
		testWalker("m.+");
		testWalker("-1", "-(1)");
		testWalker("-t[5]","-(t[5])");
		testWalker("-5 + a", "-(5).+(a)");
		testWalker("/");
		testWalker("/.at");
	    testWalker("~.test");
	}
	
	public void testLiteralGrammar() {
		testWalker("-12345","-(12345)");
		testWalker("1.05");
		testWalker("-5.04e-10","-(5.04E-10)");
		testWalker("\"hello  \\tworld\"");
		testWalker("[a,b,c]");
		testWalker("[]");
		testWalker("{ | x, y | x.+(y) }");
		testWalker("{ a := 2; b }");
	}
	
	public void testSplice() {
		testWalker("def f(x,@y) { 1 }");
		testWalker("def foo: x bar: @y { 1 }", "def foo:bar:(x, @y) { 1 }");
		testWalker("[x, @y] := t");
		testWalker("f(1,@[2,3])");
		testWalker("foo: 1 bar: @[2,3]", "foo:bar:(1, @[2,3])");
		testWalker("[x, @[y,z], u, @v]");
		testWalker("[x, @y] := [u, v, w]");
	}
	
	public void testCurriedInvocations() {
	    testWalker("closures.at(closures.length)()");
	    testWalker("closures[closures.length]()");
	}
	
	public void testOptionalArgs() {
		testWalker("def foo(x := 5) { 1 }");
		testWalker("def foo(x, y := 1+2, @z) { 1 }", "def foo(x, y := 1.+(2), @z) { 1 }");
	    testWalker("def foo(x := #5) { 1 }", "def foo(x := #(5)) { 1 }");
	}

}
