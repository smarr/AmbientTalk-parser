header { package edu.vub.at.parser; }

class NATParser extends Parser;

options {
  k = 2;
  buildAST = true;
}

{ /* begin Parser class preamble */

// The keywords2canonical auxiliary function transforms keyworded message sends or parameter lists into
// their canonical equivalent.
// EXAMPLE:
//  <foo: x bar: y> is parsed as:
// p = ( foo: ( symbol x ) ) ( bar: ( symbol y ) )
//  where p.getText() = foo:
//        p.getFirstChild() = (symbol x)
//        p.getNextSibling() = (bar: (symbol y))
// at each step in the algorithm, the current keyword is appended to the previously parsed keywords,
// while the only child of the current tree (the argument variable) is added to the arguments
// After processing all keywords, an APL tree is returned whose selector is the concatenation of the keywords and whose
// argument table contains the argument variables, e.g.:
// (apply (symbol foo:bar:) (table (symbol x) (symbol y)))
AST keywords2canonical(AST keywordparameterlist) {
	AST currentKey = keywordparameterlist;
	AST arguments = currentKey.getFirstChild(); // a pointer to the very first argument to which subsequent arguments are attached as siblings
	AST lastArgument = arguments;
	AST composedSelectorToken = new antlr.CommonAST();
	composedSelectorToken.setType(NAM);
	java.lang.StringBuffer composedSelector = new java.lang.StringBuffer(currentKey.getText());
	while (currentKey.getNextSibling() != null) {
	  currentKey = currentKey.getNextSibling();
	  composedSelector.append(currentKey.getText());
	  lastArgument.setNextSibling(currentKey.getFirstChild());
	  lastArgument = lastArgument.getNextSibling();
	}
	composedSelectorToken.setText(composedSelector.toString());
	// return #([AGAPL, "apply"], #([AGSYM,"symbol"], composedSelectorToken), #([AGTAB,"table"], arguments));
	return (AST)astFactory.make( (new ASTArray(3)).add(astFactory.create(AGAPL,"apply")).add((AST)astFactory.make( (new ASTArray(2)).add(astFactory.create(AGSYM,"symbol")).add(composedSelectorToken))).add((AST)astFactory.make( (new ASTArray(2)).add(astFactory.create(AGTAB,"table")).add(arguments))));
};

} /* end Parser class preamble */

// Ambienttalk/2 programs consist of statements separated by semicolons
program : semicolonlist EOF;

// Any list of statements separated by semicolons can be seen as arguments to a begin native.
// TODO: allow an optional final semicolon
semicolonlist : statement (SMC! statement)* { #semicolonlist = #([AGBEGIN,"begin"], #semicolonlist); };

// Statements can be either definitions assignments or ordinary expressions
statement: ("def"! definition)
         | (variable EQL) => varassignment
         | (assignment) => assignment
         | expression;

// Definitions start with ambienttalk/2's only reserved word def
// def <name> := <expression> defines a variable which can be assigned later.
// def <apl> { <body> } defines an immutable function.
// def <name>[size-exp] { <init-expression> } defines and initializes a new table of a given size
definition!: nam:variable EQL val:expression { #definition = #([AGDEFFIELD,"define-field"], nam, val); }
           | inv:signature LBC bdy:semicolonlist RBC { #definition = #([AGDEFFUN,"define-function"], inv, bdy); }
           | tbl:variable LBR siz:expression RBR LBC init:expression RBC { #definition = #([AGDEFTABLE,"define-table"], tbl, siz, init); }
           | par:parametertable EQL vls:expression { #definition = #([AGMULTIDEF,"multi-def"], par, vls); };

// A function signature can either be a canonical parameter list of the form <fun(a,b,c)>
// or a keyworded list of the form <foo: a bar: b>
signature: canonicalparameterlist
         | keywordparameterlist;

canonicalparameterlist!: var:variable LPR pars:parameterlist RPR { #canonicalparameterlist = #([AGAPL,"apply"], var, pars); }; 

// See the documentation at the keywordlist rule for more information. The difference between
// keywordparameterlist and keywordlist lies in the ability to either parse a varlist or a commalist.
keywordparameterlist: (keywordparam)+ {
	#keywordparameterlist = keywords2canonical(#keywordparameterlist);
};

keywordparam: KEY^ parameter;

// Assignment of a variable is similar to its definition albeit without the word def.
// TODO tabulation assignment requires the parser to look ahead arbitrarily far for a ':=' -> inefficient 
varassignment!: var:variable EQL val:expression { #varassignment = #([AGASSVAR, "var-set"], var, val); };

// an assignment covers both table assignment t[i] := v and field assignment o.m := v
assignment!: (parametertable EQL) => par:parametertable EQL val:expression { #assignment = #([AGMULTIASS,"multi-set"], par, val); }
           | o:operand a:assign_table_or_field[#o] { #assignment = #a; };
assign_table_or_field![AST functor]: tbl:tabulation[functor] EQL tvl:expression { #assign_table_or_field = #([AGASSTAB,"table-set"], tbl, tvl); }
                                   | sel:selection[functor]  EQL fvl:expression { #assign_table_or_field = #([AGASSFLD,"field-set"], sel, fvl); };

// Expressions are split up according to precedence. Ambienttalk/2's keyworded message
// sends have lowest priority and are therefore the highest applicable rule.
expression: keywordlist
          | rcv:comparand (opr:CMP^ arg:comparand)*;

// Comparands are expression types delimited by comparators so that they can 
// be composed of additive expressions or any higher ranking operations.
comparand: term (ADD^ term)*;

// Terms are expression types delimited by additive operators so that they can 
// be composed of multiplicative expressions or any higher ranking operations.
term: factor (MUL^ factor)*;

// Factors are expression types delimited by multiplicative operators so that they can
// be composed of exponential expressions or any higher ranking operations.
factor: invocation (POW^ invocation)*;

// Terms are expression types delimited by exponential  operators so that they can
// be composed of curried invocations only. To allow them to intervene the result 
// of parsing the reference is passed to the curried invocation.
invocation!: o:operand c:curried_invocation[#o] { #invocation = #c; };

// Operands are the most fundamental elements of the language, namely primitive 
// values (numbers and  strings), variables, blocks, inline tables and subexpressions
// A reference can also be a quotation or a first-class message creation operation
operand  :! nbr:NBR { #operand = #([AGNBR,"number"],nbr); }
         |! frc:FRC { #operand = #([AGFRC,"fraction"],frc); }
         |! txt:TXT { #operand = #([AGTXT,"text"],txt); }
         | unary
         | pseudovariable
         | symbol
         | BQU! quotation
         | HSH! unquotation
         | DOT! message
         | ARW! asyncmessage
         | LPR! subexpression
         | LBC! block
         | LBR! table;

unary! : (operator LPR) => var:operator { #unary = #var; }
       | opr:operator arg:invocation { #unary = #([AGAPL,"apply"], opr, #([AGTAB,"table"], arg)); };

// A quotation is a quoted piece of source code:
// `( statement )
quotation!: LPR stmt:statement RPR { #quotation = #([AGQUO,"quote"],stmt); };

// An unquotation is an unquoted or unquote-spliced piece of source code
// #( expression )
// #@( expression )
// TODO: appears to introduce ambiguity for blocks when using ,(...) and ,@(...)
unquotation!: LPR uexp:expression RPR { #unquotation = #([AGUNQ,"unquote"], uexp); }
            | CAT LPR usexp:expression RPR { #unquotation = #([AGUQS,"unquote-splice"], usexp); };

// Curried invocations eagerly consume all subsequent ( [ . tokens. If such tokens are
// available a single invocation is parsed passing on the received functor (which will
// be applied, tabulated, or sent a message). The result of this parsing step is passed
// to be curried even further. When no appropriate tokens are left, the passed functor 
// is returned.
curried_invocation![AST functor]:
      (LPR|LBR|DOT|ARW) => i:invoke_expression[functor] c:curried_invocation[#i] { #curried_invocation = #c; }
	| {#curried_invocation = #functor; };

// Invocation expressions are a single curried expression whether to apply, tabulate or
// invoke its functor. 
invoke_expression[AST functor]:
	 ! LPR args:commalist RPR  { #invoke_expression = #([AGAPL,"apply"], functor, args); }
	|  tabulation[functor]
	|! (DOT variable LPR | DOT KEY) => DOT apl:application { #invoke_expression = #([AGSND,"send"], functor, #([AGMSG,"message"], apl)); }
	|  selection[functor]
	|! (ARW variable LPR | ARW KEY) => ARW snd:application { #invoke_expression = #([AGSND,"send"], functor, #([AGAMS,"async-message"], snd)); };

tabulation![AST functor]: LBR idx:expression RBR { #tabulation = #([AGTBL,"table-get"], functor, idx); };
selection![AST functor]: DOT var:variable { #selection = #([AGSEL,"select"], functor, var); };

// Function application can be done using two distinct mechanisms, either using a 
// canonical format ( foobar( a1, a2 ) ) or using keywordlists (foo: a1 bar: a2).
// The latter format is used often in conjunction with blocks.
application: canonical
           | keywordlist;

canonical!: var:variable LPR args:commalist RPR { #canonical = #([AGAPL,"apply"], var, args); }; 

// Keyworded message sends are an alternation of keywords (names ending with a colon)
// and ordinary expressions. They allow for elegant ways to write control structures
// as well as custom language constructs. The keyworded messages suffer from a generalised
// version of the dangling else problem. Keywords are chained by the parser to form the
// longest possible chain. As a consequence, nested keyworded message consume all keywords
// unless they are delimited using e.g. subexpressions.
// TECH: The grammar is (inevitably?) ambiguous here, as we are aware of the problem, we
// switch off the warning for this grammar rule. 
keywordlist: singlekeyword
			 (options {
				warnWhenFollowAmbig = false;
			 } : singlekeyword)* { #keywordlist = keywords2canonical(#keywordlist); };

// This rule groups a keyword and the adjoined argument expression into a single tree element.
singlekeyword: KEY^ argument;

// First-class message creation syntax: .m() or .key:val
message!: apl:application { #message = #([AGMSG,"message"], apl); };

// First-class aynchronous message creation syntax: <-m() or <-key:val
asyncmessage!: apl:application { #asyncmessage = #([AGAMS,"async-message"], apl); };

// This rule unwraps an expression of its delimiter parentheses.
subexpression!: e:expression RPR { #subexpression = #e; };

// Inline syntax for nameless functions (lambdas or blocks)
block!: PIP pars:parameterlist PIP body:semicolonlist RBC
		 { #block = #([AGCLO, "closure"], pars, body); }
	  | no_args_body:semicolonlist RBC
		 { #block = #([AGCLO, "closure"], #([AGTAB,"table"], #([COM]) ), no_args_body); };

// Inline syntax for table expressions
table!: slots:commalist RBR { #table = #slots; };

// syntax for tables that act as the left hand side of multi definitions and assignments
parametertable!: LBR slots:parameterlist RBR { #parametertable = #slots; };

// Parses a list of expressions separated by commas. 
// USAGE: canonical function application (arguments) and inline tables
commalist: argument (COM! argument)* 
	{ #commalist = #([AGTAB,"table"], #commalist); }
	|! /* empty */ { #commalist = #([AGTAB,"table"], #([COM]));};

argument:! CAT exp:expression { #argument = #([AGSPL,"splice"], exp); }
        | expression;

// parses a list of variables that may act as formal parameters
parameterlist: parameter (COM! parameter)* { #parameterlist = #([AGTAB, "table"], #parameterlist); }
            |! /* empty */ { #parameterlist = #([AGTAB,"table"], #([COM])); };

parameter: variable_or_quotation
         |! CAT v:variable_or_quotation { #parameter = #([AGSPL,"splice"], v); };

variable_or_quotation: variable
                     | HSH! unquotation;

// user-definable names for variables
variable: symbol
        | operator;
         
symbol!: var:NAM { #symbol = #([AGSYM,"symbol"], var); };
         
pseudovariable!: "self" { #pseudovariable = #[AGSLF,"self"]; }
               | "super" { #pseudovariable = #([AGSUP, "super"]); };

operator!: cmp:CMP { #operator = #([AGCMP, "symbol"], cmp); }
         | add:ADD { #operator = #([AGADD, "symbol"], add); }
         | mul:MUL { #operator = #([AGMUL, "symbol"], mul); }
         | pow:POW { #operator = #([AGPOW, "symbol"], pow); };



class NATLexer extends Lexer;

options {
  k = 3;
}

// Protected Scanner Tokens

// OUTPUT TOKENS
// These tokens are never produced by the scanner itself as they are protected. 
// However they are used to annotate the resulting ANTLR tree, so that the walker
// can easily produce the correct Java ATAbstractGrammar elements.
// Each token definition aligns the token with its printed representation.
// Statements
protected AGBEGIN   : "begin";         // AGBegin(TAB stmts)
// Definitions
protected AGDEFFIELD: "define-field";  // AGDefField(SYM nam, EXP val)
protected AGDEFFUN  : "define-function";// AGDefFunction(SYM sel, TAB arg, BGN bdy)
protected AGDEFTABLE: "define-table";  // AGDefTable(SYM tbl, EXP siz, EXP ini)
protected AGMULTIDEF: "multi-def";     // AGMultiDefinition(TAB par, EXP val)
// Assignments
protected AGASSVAR  : "var-set";       // AGAssignField(SYM nam, EXP val)
protected AGASSTAB  : "table-set";     // AGAssignTable(EXP tbl, EXP idx, EXP val)
protected AGASSFLD  : "field-set";     // AGAssignField(EXP rcv, SYM fld, EXP val)
protected AGMULTIASS: "multi-set";     // AGMultiAssignment(TAB par, EXP val)
// Expressions
protected AGSND     : "send";          // AGMessageSend(EXP rcv, MSG msg)
protected AGAPL     : "apply";         // AGApplication(SYM sel, TAB arg)
protected AGSEL     : "select";        // AGSelection(EXP rcv, SYM sel)
protected AGMSG     : "message";       // AGMethodInvocation(SYM sel, TAB arg)
protected AGAMS     : "async-message"; // AGAsyncMessage(SYM sel, TAB arg)
protected AGTBL     : "table-get";     // AGTabulation(EXP tbl, EXP idx)
protected AGSYM     : "symbol";        // AGSymbol(TXT nam)
protected AGSLF     : "self";          // AGSelf
protected AGSUP     : "super";         // AGSuper
protected AGQUO     : "quote";         // AGQuote(STMT stmt)
protected AGUNQ     : "unquote";       // AGUnquote(EXP exp)
protected AGUQS     : "unquote-splice";// AGUnquoteSplice(EXP exp)
protected AGSPL     : "splice";        // AGSplice(EXP exp)
// Literals
protected AGNBR     : "number";        // NATNumber(<int>)
protected AGFRC     : "fraction";      // NATFraction(<double>)
protected AGTXT     : "text";          // NATText(<String>)
protected AGTAB     : "table";         // NATTable(<ATObject[]>)
protected AGCLO     : "closure";       // NATClosure(TAB arg, BGN bdy)

// auxiliary tokens for operators
protected AGCMP     : "symbol";
protected AGADD     : "symbol";
protected AGMUL     : "symbol";
protected AGPOW     : "symbol";

protected DIGIT: '0'..'9'
    ;
    
protected LETTER: ('a'..'z'|'A'..'Z' )
    ;

protected EXPONENT: ('e' | 'E')
	;
	
protected CMPCHAR: ('<' | '=' | '>' | '~' )
	;

protected ADDCHAR: ( '+' | '-')
	;
	
protected MULCHAR: ( '*' | '/' | '\\' | '&' )
	;
	
protected POWCHAR: ( '^' | '!' | '?' | '%' )
	;
	
protected OPRCHAR: CMPCHAR | ADDCHAR | MULCHAR | POWCHAR
	;
	
protected SIGN: ('+' | '-' )
	;
	
protected SCALE: EXPONENT (SIGN)? NBR
	;
	
protected COLON: ':'
	;
	
protected NBR: (DIGIT)+
	;

protected FRC: NBR (SCALE | DOT NBR (SCALE)?)
	;
	
NBR_OR_FRC options { paraphrase = "a number or fraction"; }: ( NBR EXPONENT ) => FRC  { $setType(FRC); }
          | ( NBR DOT ) => FRC       { $setType(FRC); }
          |   NBR                    { $setType(NBR); }
    ;

protected CMP: CMPCHAR (OPRCHAR)*
	;

ADD options { paraphrase = "an additive operator"; }: ADDCHAR (OPRCHAR)*
	;

MUL options { paraphrase = "a multiplicative operator"; }: MULCHAR (OPRCHAR)*
	;

POW options { paraphrase = "an exponential operator"; }: POWCHAR (OPRCHAR)*
    ;

protected NAM: LETTER (DIGIT | LETTER)*
	;
	
protected KEY: NAM COLON
    ;

NAM_OR_KEY options { paraphrase = "a name or a keyword"; }: ( NAM COLON ) => KEY  { $setType(KEY); }
          |   NAM                 { $setType(NAM); }
    ;

WHITESPACE: ('\t' |  ' ')
           { $setType(Token.SKIP); }
    ;
    
NEWLINE:  ( "\r\n" | '\r' | '\n')
          { newline(); 
            $setType(Token.SKIP); }
    ;
    
LPR options { paraphrase = "a left parenthesis"; }: '(';
RPR options { paraphrase = "a right parenthesis"; }: ')';

LBR options { paraphrase = "a left bracket"; }: '[';
RBR options { paraphrase = "a right bracket"; }: ']';

LBC options { paraphrase = "a left brace"; }: '{';
RBC options { paraphrase = "a right brace"; }: '}';

COM options { paraphrase = "a comma"; }: ',';
SMC options { paraphrase = "a semicolon"; }: ';';

EQL options { paraphrase = "an assignment"; }: ":=";
DOT options { paraphrase = "a selection"; }: '.';
protected ARW: "<-";
PIP options { paraphrase = "a block argument list"; }: '|';

BQU options { paraphrase = "a quotation"; }: '`';
HSH options { paraphrase = "an unquotation"; }: '#';
CAT options { paraphrase = "a splice"; }: '@';

CMP_OR_ARW options { paraphrase = "a comparator or asynchronous send"; }
          : ( "<-" ) => ARW  { $setType(ARW); }
          |   CMP            { $setType(CMP); }
          ;


TXT options { paraphrase = "a text string"; }: '"' (ESC|~('"'|'\\'|'\n'|'\r'))* '"'
	;

    // Single-line comments
SL_COMMENT options { paraphrase = "a single-line comment"; }
	:	"//" WHITESPACE
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?)
		{$setType(Token.SKIP); newline();}
	;

// multiple-line comments
ML_COMMENT options { paraphrase = "a multi-line comment"; }
	:	"/*" WHITESPACE
		(	/*	'\r' '\n' can be matched in one alternative or by matching
				'\r' in one iteration and '\n' in another. I am trying to
				handle any flavor of newline that comes in, but the language
				that allows both "\r\n" and "\r" and "\n" to all be valid
				newline is ambiguous. Consequently, the resulting grammar
				must be ambiguous. I'm shutting this warning off.
			 */
			options {
				generateAmbigWarnings=false;
			}
		:
			{ LA(2)!='/' }? '*'
		|	'\r''\n'		{newline();}
		|	'\r'			{newline();}
		|	'\n'			{newline();}
		|	~('*'|'\n'|'\r')
		)*
		"*/"
		{$setType(Token.SKIP);}
	;
	
protected ESC
	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	'0'..'3'
			(
				options {
					warnWhenFollowAmbig = false;
				}
			:	'0'..'7'
				(
					options {
						warnWhenFollowAmbig = false;
					}
				:	'0'..'7'
				)?
			)?
		|	'4'..'7'
			(
				options {
					warnWhenFollowAmbig = false;
				}
			:	'0'..'7'
			)?
		)
	;
	
{ import edu.vub.at.objects.ATObject;
  import edu.vub.at.objects.natives.*;
  import edu.vub.at.objects.grammar.*;
  import edu.vub.at.objects.natives.grammar.*;
  import java.util.LinkedList; }
class NATTreeWalker extends TreeParser;
{ // begin TreeWalker preamble

  // this auxiliary function converts operator syntax such as <a+b> into a message send of the form <a.+(b)>
  public AGMessageSend operatorToSend(AST opr, ATExpression receiver, ATExpression operand) {
	  return new AGMessageSend(receiver,
	                           new AGMethodInvocationCreation(AGSymbol.alloc(NATText.atValue(opr.getText())),
  	                                                          new NATTable(new ATObject[] { operand })));
  }

} // end TreeWalker preamble

program returns [NATAbstractGrammar ag] { ag = null; }
          : ag=begin
          ;

statement returns [ATStatement stmt] { stmt = null; }
          : stmt=definition
          | stmt=assignment
          | stmt=expression
          ;

definition returns [ATDefinition def]
  { def = null;
  	ATSymbol nam;
  	NATTable pars;
  	ATExpression idx, val;
  	ATBegin bdy; }
          : #(AGDEFFIELD nam=symbol val=expression) { def = new AGDefField(nam, val); }
          | #(AGDEFFUN #(AGAPL nam=symbol pars=table) bdy=begin) { def = new AGDefFunction(nam, pars, bdy); }
          | #(AGDEFTABLE nam=symbol idx=expression val=expression) { def = new AGDefTable(nam,idx,val); }
          | #(AGMULTIDEF pars=table val=expression) { def = new AGMultiDefinition(pars,val); }
          ;

assignment returns [ATAssignment ass]
  { ass = null;
    ATSymbol nam;
    ATExpression rcv, val, idx;
    NATTable par; }
          : #(AGASSVAR nam=symbol val=expression) { ass = new AGAssignVariable(nam, val); }
          | #(AGASSTAB #(AGTBL rcv=expression idx=expression) val=expression) { ass = new AGAssignTable(rcv, idx, val); }
          | #(AGASSFLD #(AGSEL rcv=expression nam=symbol) val=expression) { ass = new AGAssignField(rcv, nam, val); }
          | #(AGMULTIASS par=table val=expression) { ass = new AGMultiAssignment(par, val); }
          ;

expression returns [ATExpression exp]
  { exp = null;
  	ATExpression rcv, idx, qexp;
  	ATStatement qstmt;
  	ATSymbol sel;
  	ATMessageCreation msg;
  	NATTable arg; }
          : #(AGSND rcv=expression msg=message) { exp = new AGMessageSend(rcv,msg); }
          | #(AGAPL rcv=expression arg=table) { exp = new AGApplication(rcv, arg); }
          | #(AGSEL rcv=expression sel=symbol) { exp = new AGSelection(rcv, sel); }
          | #(AGTBL rcv=expression idx=expression) { exp = new AGTabulation(rcv, idx); }
          | #(AGQUO qstmt=statement) { exp = new AGQuote(qstmt); }
          | #(AGUNQ qexp=expression) { exp = new AGUnquote(qexp); }
          | #(AGUQS qexp=expression) { exp = new AGUnquoteSplice(qexp); }
          | #(AGSPL qexp=expression) { exp = new AGSplice(qexp); }
          | exp=message
          | exp=symbol
          | exp=binop
          | exp=literal
          ;

message returns [ATMessageCreation msg]
  { msg = null;
  	ATSymbol sel; NATTable arg; }
  	      : #(AGMSG #(AGAPL sel=symbol arg=table)) { msg = new AGMethodInvocationCreation(sel,arg); }
  	      | #(AGAMS #(AGAPL sel=symbol arg=table)) { msg = new AGAsyncMessageCreation(sel,arg); }
  	      ;
          
binop returns [ATMessageSend snd]
  { snd = null;
    ATExpression exp1, exp2; }
          : #(cmp:CMP exp1=expression exp2=expression) { snd = operatorToSend(cmp, exp1, exp2); }
          | #(add:ADD exp1=expression exp2=expression) { snd = operatorToSend(add, exp1, exp2); }
          | #(mul:MUL exp1=expression exp2=expression) { snd = operatorToSend(mul, exp1, exp2); }
          | #(pow:POW exp1=expression exp2=expression) { snd = operatorToSend(pow, exp1, exp2); }
          ;
          
literal returns[ATExpression lit]
  { lit = null;
  	NATTable par;
  	ATBegin body; }
          : #(AGNBR nbr:NBR) { lit = NATNumber.atValue(Integer.parseInt(nbr.getText())); }
          | #(AGFRC frc:FRC) { lit = NATFraction.atValue(Double.parseDouble(frc.getText())); }
          | #(AGTXT txt:TXT) { lit = NATText.atValue(txt.getText()); }
          | lit=table
          | #(AGCLO par=table body=begin) { lit = new AGClosureLiteral(par, body); }
          ;
          
symbol returns [AGSymbol sym] { sym = null; }
          : #(AGSYM txt:NAM) { sym = AGSymbol.alloc(NATText.atValue(txt.getText())); }
          | #(AGCMP cmp:CMP) { sym = AGSymbol.alloc(NATText.atValue(cmp.getText())); }
          | #(AGADD add:ADD) { sym = AGSymbol.alloc(NATText.atValue(add.getText())); }
          | #(AGMUL mul:MUL) { sym = AGSymbol.alloc(NATText.atValue(mul.getText())); }
          | #(AGPOW pow:POW) { sym = AGSymbol.alloc(NATText.atValue(pow.getText())); }
          | AGSLF { sym = AGSelf._INSTANCE_; }
          | AGSUP { sym = AGSuper._INSTANCE_; }
          ;

table returns [NATTable tab]
  { tab = null;
  	ATExpression expr;
  	LinkedList list = new LinkedList(); }
          : #(AGTAB (expr=expression { list.add(expr); })* )
              { tab = (list.isEmpty()) ? NATTable.EMPTY : new NATTable((ATObject[]) list.toArray(new ATObject[list.size()])); }
          ;
          
begin returns [AGBegin bgn]
  { bgn = null;
  	ATStatement stmt;
  	LinkedList list = new LinkedList(); }
          : #(AGBEGIN (stmt=statement { list.add(stmt); })+ )
              { bgn = new AGBegin(new NATTable((ATObject[]) list.toArray(new ATObject[list.size()]))); }
          ;