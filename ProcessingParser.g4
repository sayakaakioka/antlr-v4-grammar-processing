/*
 Copyright 2022 Sayaka Akioka

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

parser grammar ProcessingParser;

options {
	tokenVocab = ProcessingLexer;
}

compilationUnit:
	importDeclaration* (
		varDecl
		| funcDecl
		| statement
		| classDecl
	)+ EOF;

importDeclaration: IMPORT qualifiedName (DOT MUL)? SEMI;

qualifiedName: IDENTIFIER (DOT IDENTIFIER)*;

varDecl:
	modifier* type variableDeclarators SEMI;

modifier: PUBLIC | PRIVATE | FINAL | STATIC;

variableDeclarators:
	variableDeclarator (COMMA variableDeclarator)*;

variableDeclarator:
	variableDeclaratorId (ASSIGN variableInitializer)?;

variableDeclaratorId: id = IDENTIFIER (LBRACK RBRACK)*;

variableInitializer: arrayInitializer | expression;

arrayInitializer:
	LBRACE (variableInitializer (COMMA variableInitializer)* COMMA?)? RBRACE;

funcDecl:
	modifier* typeOrVoid funcId = IDENTIFIER formalParameters
	(THROWS qualifiedNameList)? (block | SEMI);

typeOrVoid: type | VOID;

type: (classType | primitiveType) (LBRACK RBRACK)*;

primitiveType:
	BOOLEAN
	| BYTE
	| CHAR
	| COLOR
	| DOUBLE
	| FLOAT
	| INT
	| LONG;

classType:
	id=IDENTIFIER typeArguments?
	| qualifiedName;

typeArguments: LT type (COMMA type)* GT;

formalParameters:
    LPAREN formalParameterList? RPAREN;

formalParameterList: formalParameter (COMMA formalParameter)*;

formalParameter:
	modifier* type variableDeclaratorId;

qualifiedNameList: qualifiedName (COMMA qualifiedName)*;

block: LBRACE blockStatement* RBRACE;

blockStatement: varDecl | statement;

classDecl:
	classModifier* CLASS id = IDENTIFIER typeParameters?
	(EXTENDS type)? (IMPLEMENTS typeList)? classBody;

classModifier: modifier | ABSTRACT;

typeParameters: LT typeParameter (COMMA typeParameter)* GT;

typeParameter: id = IDENTIFIER (EXTENDS typeBound)?;

typeBound: type (BITAND type)*;

typeList: type (COMMA type)*;

classBody: LBRACE classBodyDeclaration* RBRACE;

classBodyDeclaration:
	SEMI
	| STATIC? block
	| modifier* memberDeclaration
	| abstractMethodDeclaration;

abstractMethodDeclaration:
	classModifier* modifier* typeOrVoid id = IDENTIFIER formalParameters
	(THROWS qualifiedNameList)?;

memberDeclaration:
	funcDecl
	| genericMethodDeclaration
	| varDecl
	| constructorDeclaration
	| genericConstructorDeclaration;

genericMethodDeclaration: typeParameters funcDecl;

constructorDeclaration:
	id = IDENTIFIER formalParameters (THROWS qualifiedNameList)? block;

genericConstructorDeclaration:
	typeParameters constructorDeclaration;

statement:
	block
	| IF LPAREN expression RPAREN statement (ELSE statement)?
	| FOR LPAREN forControl RPAREN statement
	| WHILE LPAREN expression RPAREN statement
	| DO statement WHILE LPAREN expression RPAREN SEMI
	| TRY block catchClause+
	| SWITCH LPAREN expression RPAREN LBRACE switchBlockStatementGroup* switchLabel* RBRACE
	| RETURN expression? SEMI
	| THROW expression SEMI
	| BREAK SEMI
	| CONTINUE SEMI
	| expression? SEMI
	| identifierLabel = IDENTIFIER COLON statement;

forControl:
	forInit? SEMI expression? SEMI expressionList?
	| enhancedForControl;

forInit:
	modifier* type variableDeclarators
	| expressionList;

expressionList: expression (COMMA expression)*;

enhancedForControl:
	modifier* type variableDeclaratorId COLON expression;

catchClause: CATCH LPAREN modifier* catchType id = IDENTIFIER RPAREN block;

catchType: qualifiedName (BITOR qualifiedName)*;

switchBlockStatementGroup: switchLabel+ blockStatement+;

switchLabel:
	CASE (
		expression
		| enumConstantName = IDENTIFIER
		| type varName = IDENTIFIER
	) COLON
	| DEFAULT COLON;

expression:
	primary
	| LPAREN typeBound RPAREN expression
	| expression LBRACK expression RBRACK
	| expression bop = DOT (
		id = IDENTIFIER
		| funcCall
		| THIS
		| SUPER superSuffix
		| EQUALS LPAREN expressionList? RPAREN
		| nonWildcardTypeArguments explicitGenericInvocationSuffix
	)
	| funcCall
	| NEW creator
	| expression postfix = (INC | DEC)
	| prefix = (ADD | SUB | INC | DEC | BANG) expression
	| expression bop = (MUL | DIV | MOD) expression
	| expression bop = (ADD | SUB) expression
	| expression bop = (L_SHIFT | R_SHIFT) expression
    | expression bop = (
		LE | GE | GT | LT
	) expression
	| expression bop = INSTANCEOF type
	| expression bop = (EQUAL | NOTEQUAL) expression
	| expression bop = BITAND expression
	| expression bop = BITOR expression
	| expression bop = AND expression
	| expression bop = OR expression
	| <assoc = right> expression bop = QUESTION expression COLON expression
	| <assoc = right> expression bop = (
		ASSIGN | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN
	) expression
	;

primary:
	LPAREN expression RPAREN
	| THIS
	| SUPER
	| literal
	| id = IDENTIFIER
	| typeOrVoid DOT CLASS
	| nonWildcardTypeArguments explicitGenericInvocationSuffix;

literal:
	DECIMAL_LITERAL
	| FLOAT_LITERAL
	| HEX_LITERAL
	| CHAR_LITERAL
	| STRING_LITERAL
	| BOOL_LITERAL
	| COLOR_LITERAL
	| NULL_LITERAL;

nonWildcardTypeArguments: LT typeList GT;

superSuffix:
	arguments
	| DOT typeArguments? id = IDENTIFIER arguments?;

explicitGenericInvocationSuffix:
	SUPER superSuffix
	| id = IDENTIFIER arguments;

arguments: LPAREN expressionList? RPAREN;

funcCall:
	id = IDENTIFIER LPAREN expressionList? RPAREN
	| (THIS | SUPER | BOOLEAN | BYTE | CHAR | FLOAT | INT | COLOR) LPAREN expressionList? RPAREN;

creator:
	nonWildcardTypeArguments createdName classCreatorRest
	| createdName (arrayCreatorRest | classCreatorRest);

createdName:
	createdNameUnit (DOT createdNameUnit)*
	| primitiveType;

createdNameUnit: id = IDENTIFIER typeArgumentsOrDiamond?;

typeArgumentsOrDiamond: LT GT | typeArguments;

arrayCreatorRest:
	LBRACK (
		RBRACK (LBRACK RBRACK)* arrayInitializer
		| expression RBRACK (LBRACK expression RBRACK)* (LBRACK RBRACK)*
	);

classCreatorRest: arguments classBody?;