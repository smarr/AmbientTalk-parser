/**
 * AmbientTalk/2 Project
 * CommonASTWithLines.java created on 22 nov 2009 at 15:52:45
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

/**
 * @author tvcutsem
 *
 * For the reason of existence of this class, see this
 * <a href="http://tech.puredanger.com/2007/02/01/recovering-line-and-column-numbers-in-your-antlr-ast/">blog post</a>.
 */
import antlr.CommonAST;
import antlr.Token;
import antlr.collections.AST;

public class CommonASTWithLines extends CommonAST {
     private int line = 0;
     private int column = 0;
     
     public void initialize(Token tok) {
         super.initialize(tok);
         line=tok.getLine();
         column=tok.getColumn();
     }
     public int getLine() {
    	 if (line != 0) {
    		 return line;
    	 }
    	 // if line is 0, search for a line number in a child AST
    	 // and cache it
    	 AST ast = this.getFirstChild();
    	 while (ast != null && line == 0) {
             line = ast.getLine();
             column = ast.getColumn(); // cache column as well
             ast = ast.getNextSibling();
         }
    	 
    	 return line;
     }
     public int getColumn() {
    	 if (column != 0) {
    		 return column;
    	 }
    	 // if column is 0, search for a column number in a child AST
    	 // and cache it
    	 AST ast = this.getFirstChild();
    	 while (ast != null && column == 0) {
    		 column = ast.getColumn();
             ast = ast.getNextSibling();
         }
    	 
    	 return column;
     }
}