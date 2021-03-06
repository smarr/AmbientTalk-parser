header {package edu.vub.at.parser;
import java.util.List;
import java.util.LinkedList;
}

class ParserImpl extends Parser;

options {
  k = 2;
  buildAST = true;
  defaultErrorHandler = false;
}

{ /* begin Parser class preamble */

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
program! : gsl:globalstatementlist EOF { #program = #gsl; };

// TODO: refactor duplicated code by using an extra token parameter to abstract EOF and RBC tokens
// an optional terminating semicolon is allowed
// a global statementlist must always end with EOF
globalstatementlist!: sts:globalstatements { #globalstatementlist = #([AGBEGIN,"begin"], #sts); };
globalstatements! { LinkedList l = new LinkedList(); } :
	(st:statement { l.addLast(#st); } (SMC | EOF))*
	{
		AST first = l.isEmpty() ? astFactory.create() : (AST) l.removeFirst();
		AST el = first;
		while(! l.isEmpty()) {
			AST next = (AST) l.removeFirst();
			el.setNextSibling(next);
			el = next;
		}
		#globalstatements = first;
	};

// an optional terminating semicolon is allowed
// a statementlist must always end with RBC
statementlist! { LinkedList l = new LinkedList(); } :
	RBC { #statementlist = #([AGBEGIN,"begin"], #([SMC])); }
	|  st1:statement (SMC st:statement { l.addLast(#st); } )* ((SMC)? RBC)
	{
		AST first = #st1;
		AST el = first;
		while(! l.isEmpty()) {
			AST next = (AST) l.removeFirst();
			el.setNextSibling(next);
			el = next;
		}
		#statementlist = #([AGBEGIN, "begin"], first);
	};


// Statements can be either definitions assignments or ordinary expressions
statement:  ("def"! definition)
         |! ("deftype"! nam:variable sdef:typedefinition[#nam] { #statement = #sdef; })
         |  ("import"! importstatement)
         |  (variable EQL) => varassignment
         |  (assignment) => assignment
         |  expression;

// A definition can conceptually fall into to following categories, for which the rules were expanded to avoid non-determinism 
// Field Definitions:      def name                        (:= fieldValue)?
// Keyworded Definitions:  def keyword: arg1 another: arg2 ({ bodyExpression })?
// Canonical Definitions:  def name(arg1, arg2)            ({ bodyExpression })?
// Multivalue Definitions: def [name1, name2]              (:= [value1, value2])?
// Table Definitions:      def name[ indexExpression ]     ({ initialisationExpression })?
// External Definitions:   def receiver.name				(:= fieldValue)?
//                         def receiver.name(arg1, arg2)   ({ bodyExpression })?
//                         def receiver.key: arg1          ({ bodyExpression })?
// ----------------------------------------------------------------------------------------------
// Unfortunately all of the names in the rules presented above can also contain unquotations of 
// the form #( expression ) which requires an indefinite lookahead to distinguish between e.g. a
// field and a method definition. Therefore the rules which start with "def name" we delay the
// further dispatch to the rule variable_or_method. 
// NOTE: the pseudovariable self cannot be defined directly but it can be used as a receiver for
// external definitions, a fact which is reflected by the last case in this rule. 
definition!	: par:parametertable vls:valueDefinition { #definition = #([AGMULTIDEF,"multi-def"], par, vls); }
			| sig:keywordparameterlist ann:annotation bod:methodBodyDefinition { #definition = #([AGDEFFUN,"define-function"], sig, ann, bod);}
			| nam:variable_or_assignment vom:variable_or_method[#nam] { #definition = #vom; }
			| pse:pseudovariable DOT ext:external_definition[#pse] { #definition = #ext; }
			;

// When reading a definition which starts with a name, what follows can be the definition of a 
// canonical method, a field or an external definition. The former two are handled in this rule.
// for the latter a similar problem occurs to distinguish between canonical methods and fields
// (i.e. the lookahead to identify the "(" is undefined), hence this responsibility is delegated
// the external_definition rule
variable_or_method![AST nam]
			: val:valueDefinition { #variable_or_method = #([AGDEFFIELD,"define-field"], nam, val);}
			| inv:canonicalparameterlist[#nam] ann:annotation bdy:methodBodyDefinition { #variable_or_method = #([AGDEFFUN,"define-function"], inv, ann, bdy); }
			| LBR siz:expression RBR init:methodBodyDefinition { #variable_or_method = #([AGDEFTABLE,"define-table"], nam, siz, init); }
			| DOT ext:external_definition[#nam] { #variable_or_method = #ext; }
			;

// Parses external definitions on a receiver. These external definitions can involve fields and
// cononical or keyworded message.
external_definition![AST rcv]
			: sig:keywordparameterlist ann:annotation bod:methodBodyDefinition { #external_definition = #([AGDEFEXTMTH,"define-external-method"], rcv, sig, ann, bod); }
			// Here we use an antlr lookahead expression to avoid using another splicing of a rule.
			| (variable LPR) => nme:variable inv:canonicalparameterlist[#nme] an_:annotation bdy:methodBodyDefinition { #external_definition = #([AGDEFEXTMTH,"define-external-method"], rcv, inv, an_, bdy); }
			| nam:variable val:valueDefinition { #external_definition = #([AGDEFEXTFLD,"define-external-field"], rcv, nam, val); }
			;

// allows definitions which are supposedly followed by a := sign and an expression to associate
// no value with the defined value, this value is then filled in by the treewalker to be nil
valueDefinition!
			: (SMC) => 
			| (RBC) =>
			| (EOF) =>
			| EQL val:expression { #valueDefinition = #val; }
			;

// allows definitions which are supposedly followed by a closure expression to associate no
// value with the defined value, this value is then filled in by the treewalker to be an empty closure
methodBodyDefinition!
			: (SMC) =>
			| (RBC) =>
			| (EOF) =>
			| LBC bdy:statementlist { #methodBodyDefinition = #bdy; }
			;

// def typename;  is parsed into the intermediary representation as (define-type (symbol name) (table))
// i.e. the second argument to define-type is an empty table
typedefinition![AST nam]
				: { #typedefinition = #([AGDEFTYPE,"define-type"], nam, #([AGTAB,"table"], #([COM]) )); }
                 |  SST parents:commalist { #typedefinition = #([AGDEFTYPE, "define-type"], nam, parents); }
                 ;

// import host alias a := b exclude c, d;  is parsed into the intermediary representation
// as (import (symbol host) (table (table (a b))) (table (c d)))  [with a,b,c,d = (symbol _)]
importstatement!: host:expression alias:aliasbindings exclude:excludelist
   { #importstatement = #([AGIMPORT,"import"], host, alias, exclude); }
                ;
                
aliasbindings: ("alias" unquotation) => "alias"! unquotation
             | "alias"! aliasbinding (COM! aliasbinding)* { #aliasbindings = #([AGTAB, "table"], #aliasbindings); }
             | /* nothing */ { #aliasbindings = #([AGTAB, "table"], #([COM])); }
             ;
             
aliasbinding!: (name1:importname EQL name2:importname) { #aliasbinding = #([AGTAB, "table"],name1, name2); }
             ;

excludelist: ("exclude" unquotation) => "exclude"! unquotation
           | "exclude"! importname (COM! importname)* { #excludelist = #([AGTAB, "table"], #excludelist); }
           | /* nothing */ { #excludelist = #([AGTAB, "table"], #([COM])); }
           ;

// valid import names are: normal identifiers, keywords (e.g. foo:bar:), operators, assignment symbols or an unquote
importname : keywordsymbol | variable_or_quotation
           ;

// A function definition can include a canonical parameter list of the form <(a,b,c)>
canonicalparameterlist![AST var]: LPR pars:parameterlist RPR { #canonicalparameterlist = #([AGAPL,"apply"], var, pars); }; 

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
                                   | sel:zeroArityInvocation[functor]  EQL fvl:expression { #assign_table_or_field = #([AGASSFLD,"field-set"], sel, fvl); };

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
         |! LKU sym:variable_keyword_or_assignment { #operand = #([AGLKU, "lookup"], sym); }
         | unary
         | pseudovariable
         | symbol
         | BQU! quotation
         | unquotation
         | DOT! message
         | ARW! asyncmessage
         | CAR! delegationmessage
         | USD! universalmessage
         | LPR! subexpression
         | LBC! block
         | LBR! table;

// tree cases for 'unary' operators:
// +.m(1) => send the message m to + (first case)
// -t[5] => negate the invocation t[5] (second case)
// + => third case: operator as a variable
unary! : (operator (LPR|LBR|DOT|ARW|USD)) => var:operator { #unary = #var; }
       | (operator invocation) => opr:operator arg:invocation { #unary = #([AGAPL,"apply"], opr, #([AGTAB,"table"], arg)); }
       | op:operator { #unary = #op; };

// A quotation is a quoted piece of source code:
// `{ statement }
// or `operand
// note that `{ x } parses x as a statement and returns `x
// to quote a block literal, use `({ x }), which parses { x } as a subexpression
// the rule is ambiguous as `{ x } can both be interpreted as `{ statement } or as `blockliteral
quotation: (options { generateAmbigWarnings=false; } :
             statementquotation
           |! fa:fieldAssignment { #quotation = #([AGQUO,"quote"],fa); }
           |! k:keywordsymbol { #quotation = #([AGQUO,"quote"],k); }
           | operandquotation);

//statementquotation!: LBC stmt:statement RBC { #statementquotation = #([AGQUO,"quote"],#stmt); };
statementquotation!: LBC stmts:statementlist { #statementquotation = #([AGQUOBEGIN,"quote-begin"],#stmts); };
// we need special rules for parsing quotations of field assignment symbols like `foo:=
fieldAssignment!: fas:ASSNAM { #fieldAssignment = #([AGASY,"symbol"], #fas); }
              ;
// we need special rules for parsing quotations of keywords like `foo: and `foo:bar:
keywordsymbol!: ksm:KEYSYM { #keywordsymbol = #([AGKSM,"symbol"], #ksm); }
              | key:KEY { #keywordsymbol = #([AGKEY,"symbol"], #key); }
              ;
operandquotation!: qexp:operand { #operandquotation = #([AGQUO,"quote"],#qexp); };

// An unquotation is an unquoted or unquote-spliced piece of source code
// #operand
// #@operand
unquotation!: HSH uexp:operand { #unquotation = #([AGUNQ,"unquote"], uexp); }
            | HSH CAT usexp:operand { #unquotation = #([AGUQS,"unquote-splice"], usexp); };

// Curried invocations eagerly consume all subsequent ( [ . tokens. If such tokens are
// available a single invocation is parsed passing on the received functor (which will
// be applied, tabulated, or sent a message). The result of this parsing step is passed
// to be curried even further. When no appropriate tokens are left, the passed functor 
// is returned.
curried_invocation![AST functor]:
      (LPR|LBR|DOT|SEL|ARW|USD|CAR) => i:invoke_expression[functor] c:curried_invocation[#i] { #curried_invocation = #c; }
	| {#curried_invocation = #functor; };

// Invocation expressions are a single curried expression whether to apply, tabulate or
// invoke its functor. 
invoke_expression[AST functor]:
	 ! LPR args:commalist RPR  { #invoke_expression = #([AGAPL,"apply"], functor, args); }
	|  tabulation[functor]
	// o.m(args) -> send(o, msg(apl(m,args)))
	|! (DOT variable LPR | DOT KEY) => DOT msg:message { #invoke_expression = #([AGSND,"send"], functor, msg); }
	// o.m -> send(o, msg(apl(m,[])))
	|  zeroArityInvocation[functor]
	// o.&m -> select(o,m)
	|  selection[functor]
	// o<-msg(args) -> send(o, asyncmsg(apl(msg,args)))
	|! (ARW variable LPR | ARW KEY) => ARW snd:asyncmessage { #invoke_expression = #([AGSND,"send"], functor, snd); }
	// o^msg(args) -> send(o, delegation(apl(msg,args)))
	|! (CAR variable LPR | CAR KEY) => CAR del:delegationmessage { #invoke_expression = #([AGSND,"send"], functor, del); }
	// o <+ msg -> send(o, msg)
	|! (USD expression) => USD exp:expression { #invoke_expression = #([AGSND,"send"], functor, #([AGUSD,"univ-message"], exp)); };

tabulation![AST functor]: LBR idx:expression RBR { #tabulation = #([AGTBL,"table-get"], functor, idx); };
selection![AST functor]: SEL var:variable_keyword_or_assignment { #selection = #([AGSEL,"select"], functor, var); };
// transforms o.x into o.x()
zeroArityInvocation![AST functor]: DOT var:variable {
	#zeroArityInvocation = #([AGSND,"send"], functor, #([AGFSL,"field"], var));
};

// Function application can be done using two distinct mechanisms, either using a 
// canonical format ( foobar( a1, a2 ) ) or using keywordlists (foo: a1 bar: a2).
// The latter format is used often in conjunction with blocks.
// Note also that a canonical message send can be annotated, i.e. o.m(arg)@annotation-expression
application![int tokenType, String tokenText]: c:canonical a:annotation { #application = #([tokenType,tokenText], c, a); }
                                             | k:keywordlist { #application = #([tokenType,tokenText], k); };

canonical!: var:variable LPR args:commalist RPR { #canonical = #([AGAPL,"apply"], var, args); }; 

annotation!: CAT exp:expression { #annotation = #exp; }
           | /* EMPTY */
           ;

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

// First-class message creation syntax: .m or .m() or .key:val
message: (KEY | (variable LPR)) => apl:application[AGMSG,"message"] //{ #message = #([AGMSG,"message"], apl); };
       |! var:variable { #message = #([AGFSL,"field"], var); };

// First-class aynchronous message creation syntax: <-m() or <-key:val
asyncmessage: apl:application[AGAMS, "async-message"]; //{ #asyncmessage = #([AGAMS,"async-message"], apl); };

// First-class delegation message creation syntax: ^m() or ^key:val
delegationmessage: apl:application[AGDEL, "delegate"]; //{ #delegationmessage = #([AGDEL,"delegate"], apl); };

// First-class universal message: <+ expression
universalmessage!: exp:expression { #universalmessage = #([AGUSD,"univ-message"], exp); };

// This rule unwraps an expression of its delimiter parentheses.
subexpression!: e:expression RPR { #subexpression = #e; };

// Inline syntax for nameless functions (lambdas or blocks)
block!: PIP pars:parameterlist PIP body:statementlist
		 { #block = #([AGCLO, "closure"], pars, body); }
	  | no_args_body:statementlist
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

parameter!: (variable_or_quotation EQL) => var:variable_or_quotation EQL exp:expression { #parameter = #([AGASSVAR, "var-set"], var, exp); }
          | vq:variable_or_quotation { #parameter = #vq; }
          | CAT v:variable_or_quotation { #parameter = #([AGSPL,"splice"], v); };

variable_keyword_or_assignment: variable_or_assignment
                              | keywordsymbol;

variable_or_quotation: (HSH) => unquotation
                     | variable_or_assignment;
                     
variable_or_assignment: fieldAssignment
                      | variable
                      ;

// user-definable names for variables
variable:  symbol
        |  operator
        |! HSH! uexp:operand { #variable = #([AGUSM,"unquote-symbol"], uexp); }
        ;
         
symbol: kwsymbol
      | namsymbol
      ;

kwsymbol!: var:KEYSYM { #kwsymbol = #([AGKSM,"symbol"], var); };

namsymbol!: var:NAM { #namsymbol = #([AGSYM,"symbol"], var); };
         
pseudovariable!: "self" { #pseudovariable = #[AGSLF,"self"]; };

operator!: cmp:CMP { #operator = #([AGCMP, "symbol"], cmp); }
         | add:ADD { #operator = #([AGADD, "symbol"], add); }
         | mul:MUL { #operator = #([AGMUL, "symbol"], mul); }
         | pow:POW { #operator = #([AGPOW, "symbol"], pow); };



class LexerImpl extends Lexer;

options {
  k = 3;
  charVocabulary='\u0000'..'\uFFFE';
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
protected AGDEFEXTMTH: "define-external-method";// AGDefExternalMethod(SYM rcv, SYM sel, TAB arg, BGN bdy)
protected AGDEFEXTFLD: "define-external-field"; // AGDefExternalField(SYM rcv, SYM nam, EXP val)
protected AGDEFTYPE: "define-type"; // AGDefType(SYM nam, TAB parentExps)
protected AGIMPORT: "import"; // AGImport(EXP host, TAB aliases, TAB excludes)
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
protected AGLKU     : "lookup";	       // AGLookup(SYM sel)
protected AGMSG     : "message";       // AGMethodInvocation(SYM sel, TAB arg, [])
protected AGFSL     : "field";         // AGFieldSelection(SYM sel, [])
protected AGAMS     : "async-message"; // AGAsyncMessage(SYM sel, TAB arg, [])
protected AGDEL     : "delegate";      // AGDelegationCreation(SYM sel, TAB arg, [])
protected AGUSD     : "univ-message";  // ATExpression(exp)
protected AGTBL     : "table-get";     // AGTabulation(EXP tbl, EXP idx)
protected AGSYM     : "symbol";        // AGSymbol(TXT nam)
protected AGASY     : "symbol";        // AGAssignmentSymbol(TXT nam)
protected AGSLF     : "self";          // AGSelf
//protected AGSUP     : "super";         // AGSuper
protected AGQUO     : "quote";         // AGQuote(STMT stmt)
protected AGQUOBEGIN: "quote-begin";   // AGQuote(BGN stmts)
protected AGUNQ     : "unquote";       // AGUnquote(EXP exp)
protected AGUSM     : "unquote-symbol";// AGUnquoteSymbol(EXP exp)
protected AGUQS     : "unquote-splice";// AGUnquoteSplice(EXP exp)
protected AGSPL     : "splice";        // AGSplice(EXP exp)
// Literals
protected AGNBR     : "number";        // NATNumber(<int>)
protected AGFRC     : "fraction";      // NATFraction(<double>)
protected AGTXT     : "text";          // NATText(<String>)
protected AGTAB     : "table";         // NATTable(<ATObject[]>)
protected AGCLO     : "closure";       // NATClosure(TAB arg, BGN bdy)

// auxiliary tokens for operators and keyworded symbols
protected AGCMP     : "symbol";
protected AGADD     : "symbol";
protected AGMUL     : "symbol";
protected AGPOW     : "symbol";
protected AGKEY     : "symbol";
protected AGKSM     : "symbol";

protected DIGIT: '0'..'9'
    ;
    
protected LETTER: ('a'..'z'|'A'..'Z'|'_')
    ;

protected EXPONENT: ('e' | 'E')
	;

protected CMPCHAR: ( '<' | '=' | '>' | '~' )
	;

protected ADDCHAR: ( '+' | '-')
	;
	
protected MULCHAR: ( '*' | '/' | '\\' )
	;
	
protected POWCHAR: ( '!' | '?' | '%' )
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
          | ( NBR DOT NBR ) => FRC       { $setType(FRC); }
          |   NBR                    { $setType(NBR); }
    ;

protected CMP: CMPCHAR (OPRCHAR)*
             ;

ADD options { paraphrase = "an additive operator"; }: ADDCHAR (OPRCHAR)*
	;

protected MUL options { paraphrase = "a multiplicative operator"; }: MULCHAR (OPRCHAR)*
	;

POW options { paraphrase = "an exponential operator"; }: POWCHAR (OPRCHAR)*
    ;

protected NAM: LETTER (DIGIT | LETTER)*
	;

// distinguish between:
// keyworded symbols, like `foo:bar:
// keywords, like foo:
// normal identifiers, like foo
NAM_OR_KEY options { paraphrase = "a name or a keyword"; }
    : ( NAM EQL) => ASSNAM { $setType(ASSNAM); }
    | ( NAM COLON NAM ) => KEYSYM { $setType(KEYSYM); }
    | ( NAM COLON ) => KEY  { $setType(KEY); }
    |   NAM                 { $setType(NAM); }
    ;

WHITESPACE options { paraphrase = "whitespace"; }
    : ('\t' |  ' ') { $setType(Token.SKIP); }
    ;
    
NEWLINE options { paraphrase = "newline"; }
    :  ( "\r\n" | '\r' | '\n') { newline(); $setType(Token.SKIP); }
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
DOT options { paraphrase = "a dot"; }: '.';
SEL options { paraphrase = "a selection"; }: ".&";

protected ASSNAM: NAM EQL
    ;

protected KEY: NAM COLON
    ;

protected KEYSYM: (NAM COLON) (NAM COLON)*
    ;

protected ARW: "<-"; // asynchronous send operator
protected USD: "<+"; // universal send operator
protected SST: "<:"; // subtype

CAR options { paraphrase = "a delegation"; }: '^';
PIP options { paraphrase = "a block argument list"; }: '|';

BQU options { paraphrase = "a quotation"; }: '`';
HSH options { paraphrase = "an unquotation"; }: '#';
CAT options { paraphrase = "a splice"; }: '@';

LKU options { paraphrase = "a lexical lookup"; }: '&';

CMP_OR_ARW options { paraphrase = "a comparator, asynchronous or universal send"; }
          : ( "<-" ) => ARW  { $setType(ARW); }
          | ( "<+" ) => USD  { $setType(USD); }
          | ( "<:" ) => SST  { $setType(SST); }
          |   CMP            { $setType(CMP); }
          ;

TXT options { paraphrase = "a text string"; }: '"' (ESC|~('"'|'\\'))* '"'
	;

// Single-line comments
protected SL_COMMENT
    : "//" (~('\n'|'\r'))* ('\r')? ('\n')? {$setType(Token.SKIP); newline(); }
    ;

// to distinguish between operators starting with '/' and single-line
// comments starting with '//'
SL_COMMENT_OR_MUL_OPR
    : ("//") => SL_COMMENT { $setType(Token.SKIP); }
    | MUL                  { $setType(MUL); }
    ;
// multiple-line comments
ML_COMMENT options { paraphrase = "a multi-line comment"; }
	:	"/*"
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
		(	'n'  { $setText("\n"); }
		|	'r'  { $setText("\r"); }
		|	't'  { $setText("\t"); }
		|	'b'  { $setText("\b"); }
		|	'f'  { $setText("\f"); }
		|	'"'  { $setText("\""); }
		|	'\'' { $setText("\'"); }
		|	'\\' { $setText("\\"); }
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
  import edu.vub.at.objects.ATAbstractGrammar;
  import edu.vub.at.objects.natives.*;
  import edu.vub.at.objects.grammar.*;
  import edu.vub.at.objects.natives.grammar.*;
  import edu.vub.at.exceptions.XParseError;
  import edu.vub.at.exceptions.InterpreterException;
  import java.util.LinkedList; }
class TreeWalkerImpl extends TreeParser;

{ // begin TreeWalker preamble

  // this auxiliary function converts operator syntax such as <a+b> into a message send of the form <a.+(b)>
  public AGMessageSend operatorToSend(AST opr, ATExpression receiver, ATExpression operand) {
	  AGMessageSend snd = new AGMessageSend(receiver,
	                           new AGMethodInvocationCreation(AGSymbol.alloc(NATText.atValue(opr.getText())),
  	                                                          NATTable.atValue(new ATObject[] { operand }),
  	                                                          NATTable.EMPTY));
  	  locate(opr, snd);
  	  return snd;
  }
  
  public AGBegin emptyMethodBody() {
  	  return new AGBegin(NATTable.EMPTY);
  }
  
  protected String fileName_ = "";
  
  public void setFileName(String fileName) {
  	fileName_ = fileName;
  }
  
  public void locate(AST ast, ATAbstractGrammar ag) {
    ag.impl_setLocation(new SourceLocation(ast.getLine(),
                                      ast.getColumn(),
                                      fileName_));
  }

} // end TreeWalker preamble

program returns [NATAbstractGrammar ag] throws InterpreterException { ag = null; }
          : ag=begin
          ;

statement returns [ATStatement stmt] throws InterpreterException { stmt = null; }
          : stmt=definition
          | stmt=assignment
          | stmt=expression
          | stmt=importstmt
          ;

definition returns [ATDefinition def] throws InterpreterException
  { def = null;
  	ATSymbol nam, rcv;
  	NATTable pars;
  	ATExpression idx, val;
  	ATBegin bdy;
  	ATExpression ann =NATTable.EMPTY; }
          : { val = AGSymbol.jAlloc("nil"); }
            #(AGDEFFIELD nam=symbol (val=expression)?) { def = new AGDefField(nam, val); locate(#AGDEFFIELD, def); }
          | { bdy = emptyMethodBody(); } 
            #(AGDEFFUN #(AGAPL nam=symbol pars=params) (ann=expression)? (bdy=begin)? ) { def = new AGDefFunction(nam, pars, bdy, ann); locate(#AGDEFFUN, def); }
          | { bdy = emptyMethodBody(); }
            #(AGDEFEXTMTH rcv=symbol #(AGAPL nam=symbol pars=params) (ann=expression)? (bdy=begin)? ) { def = new AGDefExternalMethod(rcv, nam, pars, bdy, ann); locate(#AGDEFEXTMTH, def); }
          | { val = AGSymbol.jAlloc("nil"); }
            #(AGDEFEXTFLD rcv=symbol nam=symbol (val=expression)? )  { def = new AGDefExternalField(rcv, nam, val); locate(#AGDEFEXTFLD, def); }
          | { bdy = emptyMethodBody(); }
            #(AGDEFTABLE nam=symbol idx=expression (bdy=begin)? ) { def = new AGDefTable(nam,idx,bdy); locate(#AGDEFTABLE, def); }
          | #(AGMULTIDEF pars=params 
            { val= NATTable.ofSize(pars.elements_.length); } 
            (val=expression)? ) 
            { def = new AGMultiDefinition(pars,val); locate(#AGMULTIDEF, def); }
          | #(AGDEFTYPE nam=symbol pars=table) { def = new AGDefType(nam, pars); locate(#AGDEFTYPE, def); } // pars = the parent type tags here
          ;

importstmt returns [ATImport imp] throws InterpreterException
  { imp = null;
  	ATExpression host, aliases, excludes; }
          : #(AGIMPORT host=expression aliases=expression excludes=expression) { imp = new AGImport(host, aliases, excludes); locate(#AGIMPORT,imp); }
          ;

assignment returns [ATAssignment ass] throws InterpreterException
  { ass = null;
    ATSymbol nam;
    ATExpression rcv, val, idx;
    NATTable par; }
          : #(AGASSVAR nam=symbol val=expression) { ass = new AGAssignVariable(nam, val); locate(#AGASSVAR,ass); }
          | #(AGASSTAB #(AGTBL rcv=expression idx=expression) val=expression) { ass = new AGAssignTable(rcv, idx, val); locate(#AGASSTAB,ass); }
          | #(AGASSFLD #(AGSND rcv=expression #(AGFSL nam=symbol)) val=expression) { ass = new AGAssignField(rcv, nam, val); locate(#AGASSFLD,ass); }
          | #(AGMULTIASS par=params val=expression) { ass = new AGMultiAssignment(par, val); locate(#AGMULTIASS,ass); }
          ;

expression returns [ATExpression exp] throws InterpreterException
  { exp = null;
  	ATExpression rcv, idx, qexp, msg;
  	ATStatement qstmt;
  	ATSymbol sel;
  	NATTable arg; }
          : #(AGSND rcv=expression msg=message) { exp = new AGMessageSend(rcv,msg); locate(#AGSND,exp); }
          | #(AGAPL rcv=expression arg=table) { exp = new AGApplication(rcv, arg); locate(#AGAPL,exp); }
          | #(AGSEL rcv=expression sel=symbol) { exp = new AGSelection(rcv, sel); locate(#AGSEL,exp); }
          | #(AGTBL rcv=expression idx=expression) { exp = new AGTabulation(rcv, idx); locate(#AGTBL,exp); }
          | #(AGQUO qexp=expression) { exp = new AGQuote(qexp); locate(#AGQUO,exp); }
          | #(AGQUOBEGIN qstmt=begin) { exp = new AGQuote(qstmt); locate(#AGQUOBEGIN,exp); }
          | #(AGUNQ qexp=expression) { exp = new AGUnquote(qexp); locate(#AGUNQ,exp); }
          | #(AGUQS qexp=expression) { exp = new AGUnquoteSplice(qexp); locate(#AGUQS,exp); }
          | #(AGSPL qexp=expression) { exp = new AGSplice(qexp); locate(#AGSPL,exp); }
          | #(AGLKU sel=symbol) { exp = new AGLookup(sel); locate(#AGLKU,exp); }
          | exp=message
          | exp=symbol
          | exp=binop
          | exp=literal
          ;

message returns [ATExpression msg] throws InterpreterException
  { msg = null;
  	ATExpression exp;
  	ATSymbol sel; NATTable arg; ATExpression ann = NATTable.EMPTY; }
  	      : #(AGMSG #(AGAPL sel=symbol arg=table) (ann=expression)? ) { msg = new AGMethodInvocationCreation(sel,arg,ann); locate(#AGMSG,msg); }
  	      | #(AGFSL sel=symbol (ann=expression)? ) { msg = new AGFieldSelectionCreation(sel,ann); locate(#AGFSL,msg); }
  	      | #(AGAMS #(AGAPL sel=symbol arg=table) (ann=expression)? ) { msg = new AGAsyncMessageCreation(sel,arg,ann); locate(#AGAMS,msg); }
  	      | #(AGDEL #(AGAPL sel=symbol arg=table) (ann=expression)? ) { msg = new AGDelegationCreation(sel,arg,ann); locate(#AGDEL,msg); }
  	      | #(AGUSD exp=expression) { msg=exp; }
  	      ;
          
binop returns [ATMessageSend snd] throws InterpreterException
  { snd = null;
    ATExpression exp1, exp2; }
          : #(cmp:CMP exp1=expression exp2=expression) { snd = operatorToSend(cmp, exp1, exp2); }
          | #(add:ADD exp1=expression exp2=expression) { snd = operatorToSend(add, exp1, exp2); }
          | #(mul:MUL exp1=expression exp2=expression) { snd = operatorToSend(mul, exp1, exp2); }
          | #(pow:POW exp1=expression exp2=expression) { snd = operatorToSend(pow, exp1, exp2); }
          ;
          
literal returns[ATExpression lit] throws InterpreterException
  { lit = null;
  	NATTable par;
  	ATBegin body; }
          : #(AGNBR nbr:NBR) {
          	try {
          		lit = NATNumber.atValue(Integer.parseInt(nbr.getText()));
          		locate(#AGNBR, lit);
          	} catch (NumberFormatException e) {
          		throw new XParseError("unparsable number", e);
          	}
          }
          | #(AGFRC frc:FRC) {
          	try {
          		lit = NATFraction.atValue(Double.parseDouble(frc.getText()));
          		locate(#AGFRC, lit);
          	} catch(NumberFormatException e) {
          		throw new XParseError("unparsable fraction", e);
          	}
          }
          | #(AGTXT txt:TXT) { String text = txt.getText(); lit = NATText.atValue(text.substring(1, text.length() - 1)); locate(#AGTXT,lit); }
          | lit=table
          | #(AGCLO par=params body=begin) { lit = new AGClosureLiteral(par, body); locate(#AGCLO,lit); }
          ;
          
symbol returns [ATSymbol sym] throws InterpreterException { sym = null; ATExpression exp = null; }
          : #(AGSYM txt:NAM) { sym = AGSymbol.alloc(NATText.atValue(txt.getText())); locate(#AGSYM,sym); }
          | #(AGKEY key:KEY) { sym = AGSymbol.alloc(NATText.atValue(key.getText())); locate(#AGKEY,sym); }
          | #(AGKSM ksm:KEYSYM) { sym = AGSymbol.alloc(NATText.atValue(ksm.getText())); locate(#AGKSM,sym); }
          | #(AGCMP cmp:CMP) { sym = AGSymbol.alloc(NATText.atValue(cmp.getText())); locate(#AGCMP,sym); }
          | #(AGADD add:ADD) { sym = AGSymbol.alloc(NATText.atValue(add.getText())); locate(#AGADD,sym); }
          | #(AGMUL mul:MUL) { sym = AGSymbol.alloc(NATText.atValue(mul.getText())); locate(#AGMUL,sym); }
          | #(AGPOW pow:POW) { sym = AGSymbol.alloc(NATText.atValue(pow.getText())); locate(#AGPOW,sym); }
          | #(AGASY asy:ASSNAM) { sym = AGAssignmentSymbol.jAlloc(asy.getText()); locate(#AGASY,sym); }
          | AGSLF { sym = AGSelf._INSTANCE_; }
          | #(AGUSM exp=expression) { sym = new AGUnquoteSymbol(exp); locate(#AGUSM,sym); }
          ;

param returns [ATAbstractGrammar ag] throws InterpreterException
  { ag = null;
  	ATSymbol nam; ATExpression exp; }
          : #(AGASSVAR nam=symbol exp=expression) { ag = new AGAssignVariable(nam, exp); locate(#AGASSVAR,ag); }
		  | ag=symbol
		  | #(AGUNQ exp=expression) { ag = new AGUnquote(exp); locate(#AGUNQ,ag); }
          | #(AGUQS exp=expression) { ag = new AGUnquoteSplice(exp); locate(#AGUQS,ag); }
          | #(AGSPL exp=expression) { ag = new AGSplice(exp); locate(#AGSPL,ag); }
		  ;

table returns [NATTable tab] throws InterpreterException
  { tab = null;
  	ATExpression expr;
  	LinkedList list = new LinkedList(); }
          : #(AGTAB (expr=expression { list.add(expr); })* )
              { tab = (list.isEmpty()) ? NATTable.EMPTY : NATTable.atValue((ATObject[]) list.toArray(new ATObject[list.size()])); locate(#AGTAB,tab); }
          ;
          
begin returns [AGBegin bgn] throws InterpreterException
  { bgn = null;
  	ATStatement stmt;
  	LinkedList list = new LinkedList(); }
          : #(AGBEGIN (stmt=statement { list.add(stmt); })* )
              { bgn = new AGBegin((list.isEmpty()) ? NATTable.EMPTY : NATTable.atValue((ATObject[]) list.toArray(new ATObject[list.size()]))); locate(#AGBEGIN,bgn); }
          ;
          
params returns [NATTable par] throws InterpreterException
  { par = null;
  	ATAbstractGrammar formal;
  	LinkedList list = new LinkedList(); }
          : #(AGTAB (formal=param { list.add(formal); })* )
              { par = (list.isEmpty()) ? NATTable.EMPTY : NATTable.atValue((ATObject[]) list.toArray(new ATObject[list.size()])); locate(#AGTAB,par); }
          ;