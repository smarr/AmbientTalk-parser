package edu.vub.at.parser;

import edu.vub.at.objects.natives.grammar.NATAbstractGrammar;
import edu.vub.at.parser.NATLexer;
import edu.vub.at.parser.NATParser;
import edu.vub.at.parser.NATTreeWalker;

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
            NATLexer lexer = new NATLexer(new ByteArrayInputStream(input.getBytes()));
            NATParser parser = new NATParser(lexer);
            // Parse the input expression
            parser.program();
            CommonAST t = (CommonAST)parser.getAST();
            // Print the resulting tree out in LISP notation
            System.out.println(t.toStringList());
            NATTreeWalker walker = new NATTreeWalker();
            // Traverse the tree created by the parser
            NATAbstractGrammar ag = walker.program(t);
            assertEquals(output.replaceAll("\\s",""), ag.meta_print().javaValue.replaceAll("\\s",""));
        } catch(Exception e) {
            fail("exception: "+e);
        }
	}
	
	public void testStatementGrammar() {
		testWalker("a; b; c");
		testWalker("def x := 5");
		testWalker("def f(a,b) { 5 }");
		testWalker("def foo: x bar: y { 5 }", "def foo:bar:(x,y) { 5 }");
		testWalker("def t[5] { a }");
		testWalker("def [x, y] := [y, x]");
		testWalker("x := 7");
		testWalker("x[5] := 7");
		testWalker("o.m := x");
		testWalker("[x, y] := [y, x]");
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
		testWalker("t[a]");
		testWalker("f()[a+b]", "f()[a.+(b)]");
		testWalker("a");
		testWalker("`(t[a])");
		testWalker("#(t[a])");
		testWalker("#@(t[a])");
	}
	
	public void testOperatorGrammar() {
		testWalker("1 + 2 + 3", "1.+(2).+(3)");
		testWalker("a * b^3 + c < d / e - f", "a.*(b.^(3)).+(c).<(d./(e).-(f))");
		testWalker("+(1,2)");
		testWalker("a.+(2)");
		testWalker("+.m(1)", "+(.m(1))");
		testWalker("-5 + a", "-(5).+(a)");
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

}
