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

import java.io.ByteArrayInputStream;

import antlr.ANTLRException;
import antlr.CommonAST;
import edu.vub.at.exceptions.XParseError;
import edu.vub.at.exceptions.XTypeMismatch;
import edu.vub.at.objects.ATAbstractGrammar;
import edu.vub.at.objects.ATText;
import edu.vub.at.objects.natives.NATNil;

/**
 * The class NATParser is a front-end (or Facade) to hide the details of the parser from
 * the ambienttalk core interpreter. It allows one to parse a string into a dedicated
 * parsetree as specified by the ambienttalk core. 
 *
 * @author smostinc
 */
public class NATParser extends NATNil {

	public static final NATParser _INSTANCE_ = new NATParser();
	
	public ATAbstractGrammar base_parse(ATText source) throws XTypeMismatch, XParseError {
		return parse(source.asNativeText().javaValue);
	}
	
	private ATAbstractGrammar parse(String source) throws XParseError {
		try {
			LexerImpl lexer = new LexerImpl(new ByteArrayInputStream(source.getBytes()));
			ParserImpl parser = new ParserImpl(lexer);
			// Parse the input expression
			parser.program();
			CommonAST t = (CommonAST)parser.getAST();
			
			// Traverse the tree created by the parser
			TreeWalkerImpl walker = new TreeWalkerImpl();
			return walker.program(t);		
		} catch(ANTLRException e) {
			throw new XParseError("Illegal input : \n" + source, e);
		}
	}
}
