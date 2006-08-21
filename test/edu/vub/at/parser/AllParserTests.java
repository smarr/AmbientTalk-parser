package edu.vub.at.parser;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllParserTests {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(AllParserTests.class);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite("AmbientTalk/2 parser and AST builder Test");
		//$JUnit-BEGIN$
		suite.addTestSuite(ATWalkerTest.class);
		suite.addTestSuite(ATParserTest.class);
		//$JUnit-END$
		return suite;
	}

}
