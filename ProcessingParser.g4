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

compilationUnit
	: importDeclaration* (varDecl | funcDecl | statement | classDecl)+ EOF;

importDeclaration
	: IMPORT qualifiedName ('.' '*')? ';'
	;

qualifiedName
	: IDENTIFIER ('.' IDENTIFIER)*
	;

varDecl
	: modifier* (classType | primitiveType) ('[' ']')* variableDeclarators ';'
	;

modifier
	: PUBLIC
    | PRIVATE
    | FINAL
	;

variableDeclarators
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
	: variableDeclaratorId ('=' variableInitializer)?
	;

variableDeclaratorId
	: IDENTIFIER ('[' ']')*
    ;

variableInitializer
	: arrayInitializer
	| expression
	;

arrayInitializer
	: '{' (variableInitializer (',' variableInitializer)* (',')? )? '}'
	;

funcDecl
	: modifier* typeOrVoid IDENTIFIER formalParameters('[' ']')* (THROWS qualifiedNameList)? funcBody
	;

typeOrVoid
	: (classType | primitiveType) ('[' ']')*
	| VOID
	;

primitiveType
	: BOOLEAN
	| BYTE
	| CHAR
	| COLOR
	| DOUBLE
	| FLOAT
	| INT
	| LONG
    | STRING
	;

classType
	: IDENTIFIER typeArguments? ('.' IDENTIFIER typeArguments?)*
	;

typeArguments
	: '<' typeArgument (',' typeArgument)* '>'
	;

typeArgument
	: (classType | primitiveType) ('[' ']')*
	;

formalParameters
	: '(' (receiverParameter?
			| receiverParameter (',' formalParameterList)?
			| formalParameterList?
			) ')'
	;

receiverParameter
	: (classType | primitiveType) ('[' ']')* (IDENTIFIER '.')* THIS
	;

formalParameterList
	: formalParameter (',' formalParameter)*
	;

formalParameter
	: modifier* (classType | primitiveType) ('[' ']')* variableDeclaratorId
	;

qualifiedNameList
	: qualifiedName (',' qualifiedName)*
	;

funcBody
	: block | ';'
	;

block
	: '{' blockStatement * '}'
	;

blockStatement
	: varDecl
	| statement
	;

classDecl
	: CLASS IDENTIFIER typeParameters?
		(EXTENDS (classType | primitiveType) ('[' ']')*)?
		(IMPLEMENTS typeList)?
		classBody
	;

typeParameters
	: '<' typeParameter (',' typeParameter)* '>'
	;

typeParameter
	: IDENTIFIER (EXTENDS typeBound)?
	;

typeBound
	: (classType | primitiveType) ('[' ']')* ('&' (classType | primitiveType) ('[' ']')* )*
	;

typeList
	: (classType | primitiveType) ('[' ']')* (',' (classType | primitiveType) ('[' ']')* )*
	;

classBody
	: '{' classBodyDeclaration* '}'
	;

classBodyDeclaration
	: ';'
	| STATIC? block
	| modifier* memberDeclaration
	;

memberDeclaration
	: funcDecl
	| genericMethodDeclaration
	| fieldDeclaration
	| constructorDeclaration
	| genericConstructorDeclaration
	;

genericMethodDeclaration
	: typeParameters funcDecl
	;

fieldDeclaration
	: varDecl
	;

constructorDeclaration
	: IDENTIFIER formalParameters (THROWS qualifiedNameList)? constructorBody=block
	;

genericConstructorDeclaration
	: typeParameters constructorDeclaration
	;


statement
    : blockLabel=block
    | IF parExpression statement (ELSE statement)?
    | FOR '(' forControl ')' statement
    | WHILE parExpression statement
    | DO statement WHILE parExpression ';'
    | TRY block catchClause+
    | SWITCH parExpression '{' switchBlockStatementGroup* switchLabel* '}'
    | RETURN expression? ';'
    | THROW expression ';'
    | BREAK ';'
    | CONTINUE ';'
    | SEMI
    | statementExpression=expression ';'
    | identifierLabel=IDENTIFIER ':' statement
    ;

parExpression
    : '(' expression ')'
    ;

forControl
    : forInit? ';' expression? ';' forUpdate=expressionList?
	| enhancedForControl
    ;

forInit
    : modifier* (classType | primitiveType) ('[' ']')* variableDeclarators
    | expressionList
    ;

expressionList
    : expression (',' expression)*
    ;

enhancedForControl
    : modifier* (classType | primitiveType) ('[' ']')* variableDeclaratorId ':' expression
    ;

catchClause
    : CATCH '(' modifier* catchType IDENTIFIER ')' block
    ;

catchType
    : qualifiedName ('|' qualifiedName)*
    ;

switchBlockStatementGroup
    : switchLabel+ blockStatement+
    ;

switchLabel
    : CASE (constantExpression=expression | enumConstantName=IDENTIFIER | (classType | primitiveType) ('[' ']')* varName=IDENTIFIER) ':'
    | DEFAULT ':'
    ;

expression
    : primary
    | expression bop='.'
      (
         IDENTIFIER
       | methodCall
       | THIS
       | NEW nonWildcardTypeArguments? innerCreator
       | SUPER superSuffix
       | explicitGenericInvocation
      )
    | expression '[' expression ']'
    | methodCall
    | NEW creator
    | '(' (classType | primitiveType) ('[' ']')* ('&' (classType | primitiveType) ('[' ']')* )* ')' expression
    | expression postfix=('++' | '--')
    | prefix=('+'|'-'|'++'|'--') expression
    | '!' expression
    | expression bop=('*'|'/'|'%') expression
    | expression bop=('+'|'-') expression
    | expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    | expression bop=('<=' | '>=' | '>' | '<') expression
    | expression bop=INSTANCEOF ((classType | primitiveType) ('[' ']')* )
    | expression bop=('==' | '!=') expression
    | expression bop='&' expression
    | expression bop='|' expression
    | expression bop='&&' expression
    | expression bop='||' expression
    | <assoc=right> expression bop='?' expression ':' expression
    | <assoc=right> expression
      bop=('=' | '+=' | '-=' | '*=' | '/=')
      expression
    ;

primary
    : '(' expression ')'
    | THIS
    | SUPER
    | literal
    | IDENTIFIER
    | typeOrVoid '.' CLASS
    | nonWildcardTypeArguments (explicitGenericInvocationSuffix | THIS arguments)
    ;

literal
    : integerLiteral
    | floatLiteral
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOL_LITERAL
	| COLOR_LITERAL
    | NULL_LITERAL
    ;

integerLiteral
    : DECIMAL_LITERAL
    ;

floatLiteral
    : FLOAT_LITERAL
    ;

nonWildcardTypeArguments
    : '<' typeList '>'
    ;

superSuffix
    : arguments
    | '.' typeArguments? IDENTIFIER arguments?
    ;

explicitGenericInvocationSuffix
    : SUPER superSuffix
    | IDENTIFIER arguments
    ;

arguments
    : '(' expressionList? ')'
    ;

explicitGenericInvocation
    : nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

innerCreator
    : IDENTIFIER nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;


nonWildcardTypeArgumentsOrDiamond
    : '<' '>'
    | nonWildcardTypeArguments
    ;

methodCall
    : IDENTIFIER '(' expressionList? ')'
    | THIS '(' expressionList? ')'
    | SUPER '(' expressionList? ')'
    | BOOLEAN '(' expressionList? ')'
    | BYTE '(' expressionList? ')'
    | CHAR '(' expressionList? ')'
    | FLOAT '(' expressionList? ')'
    | INT '(' expressionList? ')'
    | COLOR '(' expressionList? ')'
    ;

creator
    : nonWildcardTypeArguments createdName classCreatorRest
    | createdName (arrayCreatorRest | classCreatorRest)
    ;

createdName
    : IDENTIFIER typeArgumentsOrDiamond? ('.' IDENTIFIER typeArgumentsOrDiamond?)*
    | primitiveType
    ;

typeArgumentsOrDiamond
    : '<' '>'
    | typeArguments
    ;

arrayCreatorRest
    : '[' (']' ('[' ']')* arrayInitializer | expression ']' ('[' expression ']')* ('[' ']')*)
    ;

classCreatorRest
    : arguments classBody?
    ;