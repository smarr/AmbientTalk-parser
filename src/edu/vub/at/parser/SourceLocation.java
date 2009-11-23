/**
 * AmbientTalk/2 Project
 * SourceLocation.java created on 22 nov 2009 at 17:48:28
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
 * A SourceLocation remembers an AST's filename, line number and column number.
 * 
 */
public class SourceLocation implements java.io.Serializable {

	public final int line;
	public final int column;
	public final String fileName;
	
	public SourceLocation(int line, int col, String fileName) {
		this.line = line;
		this.column = col;
		this.fileName = fileName;
	}
	
	public String toString() {
		int slashIdx = fileName.lastIndexOf("/");
		return line + ":" + column + ":" +
		         (slashIdx == -1 ? fileName : fileName.substring(slashIdx+1));
	}
	
}
