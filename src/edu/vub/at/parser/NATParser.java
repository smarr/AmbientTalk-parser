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
import edu.vub.at.exceptions.XIOProblem;
import edu.vub.at.exceptions.XParseError;
import edu.vub.at.objects.ATAbstractGrammar;
import edu.vub.at.objects.ATObject;
import edu.vub.at.objects.ATText;
import edu.vub.at.objects.natives.NATByCopy;
import edu.vub.at.objects.natives.NATText;
import edu.vub.at.objects.natives.grammar.NATAbstractGrammar;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import antlr.ANTLRException;
import antlr.ASTFactory;
import antlr.CharScanner;
import antlr.RecognitionException;
import antlr.collections.AST;

/**
 * The class NATParser is a front-end (or Facade) to hide the details of the parser from
 * the ambienttalk core interpreter. It allows one to parse a string into a dedicated
 * parsetree as specified by the ambienttalk core. 
 *
 * @author smostinc
 */
public class NATParser extends NATByCopy {

	public static ParserFactory _FACTORY_ = new ParserFactory() {
		public CharScanner createLexer(InputStream source) {
			return new LexerImpl(source);
		};
		
		public AmbientTalkParser createParser(String fileName, final CharScanner lexer) {
			final ParserImpl parser_ = new ParserImpl(lexer);

			// we'll make the parser generate our custom ASTs that
			// capture line number and column number information
			// (see the CommonASTWithLines class for details)
			ASTFactory factory = new ASTFactory();
		    factory.setASTNodeClass(CommonASTWithLines.class);
		    parser_.setASTFactory(factory);
		    if (fileName != null) parser_.setFilename(fileName);
		    
			return new AmbientTalkParser() {
				public AST parseProgram() throws ANTLRException {
					parser_.program();
					return parser_.getAST();
				}
				
				public void setFilename(String name) {
					parser_.setFilename(name);
				}
			};
		};
		
		public AmbientTalkTreeWalker createTreeWalker(String fileName) {
			final TreeWalkerImpl walker_ = new TreeWalkerImpl();
			walker_.setFileName(fileName == null ? "" : fileName);

			// we'll make the parser generate our custom ASTs that
			// capture line number and column number information
			// (see the CommonASTWithLines class for details)
		    ASTFactory factory = new ASTFactory();
		    factory.setASTNodeClass(CommonASTWithLines.class);
		    walker_.setASTFactory(factory);
			
			return new AmbientTalkTreeWalker(fileName == null ? "" : fileName) {				
				public NATAbstractGrammar walkAST(AST tree) throws InterpreterException ,ANTLRException {
					return walker_.program(tree);
				};
			};
		};
	};
	
	public static final NATParser _INSTANCE_ = new NATParser();
	
	public ATAbstractGrammar base_parse(ATText source) throws InterpreterException {
		return parse("read", source.asNativeText().javaValue);
	}

	public static ATAbstractGrammar parse(String filename, InputStream source) throws InterpreterException {
		try {
			try {
				CharScanner lexer = _FACTORY_.createLexer(source);
				AmbientTalkParser parser = _FACTORY_.createParser(filename, lexer);
				AmbientTalkTreeWalker walker = _FACTORY_.createTreeWalker(filename);
				source.mark(source.available());
				
				// Parse the input expression
			    AST tree = parser.parseProgram();

				// Traverse the tree created by the parser
				return walker.walkAST(tree);
			} catch(RecognitionException e) {
				e.printStackTrace();
				source.reset();
				throw new XParseError(source, e.getMessage(), e.fileName, e.line, e.column, e);
			} catch(ANTLRException e) {
				throw new XParseError(e.getMessage(), e);
			}
	    } catch (IOException e) {
	    	throw new XIOProblem(e);
	    }
	}
	
	public static ATAbstractGrammar parse(String filename, String source) throws InterpreterException {
		return parse(filename, new ByteArrayInputStream(source.getBytes()));
	}
	
	public NATText meta_print() throws InterpreterException {
		return NATText.atValue("<native object: parser>");
	}
	
	/**
	 * After deserialization, ensure that the parser remains unique.
	 */
	public ATObject meta_resolve() throws InterpreterException {
		return NATParser._INSTANCE_;
	}
}
