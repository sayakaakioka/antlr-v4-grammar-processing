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

lexer grammar ProcessingLexer;

// Structure Keywords
LBRACK: '[';
RBRACK: ']';
ASSIGN: '=';
CATCH: 'catch';
CLASS: 'class';
COMMA: ',';
COMMENT: '//' ~[\r\n]* -> channel(HIDDEN);
LBRACE: '{';
RBRACE: '}';
DOC_COMMENT: '/**' .*? '*/' -> channel(HIDDEN);
DOT: '.';
EXTENDS: 'extends';
FINAL: 'final';
IMPLEMENTS: 'implements';
IMPORT: 'import';
BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
NEW: 'new';
LPAREN: '(';
RPAREN: ')';
PRIVATE: 'private';
PUBLIC: 'public';
RETURN: 'return';
SEMI: ';';
STATIC: 'static';
SUPER: 'super';
THIS: 'this';
TRY: 'try';
VOID: 'void';

// Data Primitive Keywords
BOOLEAN: 'boolean';
BYTE: 'byte';
CHAR: 'char';
COLOR: 'color';
DOUBLE: 'double';
FLOAT: 'float';
INT: 'int';
LONG: 'long';

// Control Relational Operators
EQUAL: '==';
GT: '>';
GE: '>=';
NOTEQUAL: '!=';
LT: '<';
LE: '<=';

// Control Iteration Keywords
FOR: 'for';
WHILE: 'while';
DO: 'do';

// Control Conditional Keywords
BREAK: 'break';
CASE: 'case';
QUESTION: '?';
COLON: ':';
CONTINUE: 'continue';
DEFAULT: 'default';
ELSE: 'else';
IF: 'if';
SWITCH: 'switch';

// Logical Operators
AND: '&&';
BANG: '!';
OR: '||';

// Math Operators
ADD_ASSIGN: '+=';
ADD: '+';
DEC: '--';
DIV: '/';
DIV_ASSIGN: '/=';
INC: '++';
SUB: '-';
MOD: '%';
MUL: '*';
MUL_ASSIGN: '*=';
SUB_ASSIGN: '-=';

// Math Bitwise Operators
BITAND: '&';
BITOR: '|';

// Java related keywords
THROWS: 'throws';
THROW: 'throw';
INSTANCEOF: 'instanceof';
ABSTRACT: 'abstract';

// Literals
DECIMAL_LITERAL: ('0' | [1-9] (Digits? | '_'+ Digits)) [lL]?;
HEX_LITERAL: '0x' [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])?;
FLOAT_LITERAL: (Digits '.' Digits? | '.' Digits) ExponentPart? [fFdD]?
	| Digits (ExponentPart [fFdD]? | [fFdD]);
COLOR_LITERAL: ('#' [0-9a-fA-F]*) | (HEX_LITERAL);
BOOL_LITERAL: 'true' | 'false';
CHAR_LITERAL: '\'' (~['\\\r\n] | EscapeSequence) '\'';
STRING_LITERAL: '"' (~["\\\r\n] | EscapeSequence)* '"';
NULL_LITERAL: 'null';

// Identifiers
IDENTIFIER: Letter LetterOrDigit*;

// Fragment rules
fragment ExponentPart: [eE] [+-]? Digits;
fragment EscapeSequence:
	'\\' [btnfr"'\\]
	| '\\' ([0-3]? [0-7])? [0-7]
	| '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit;
fragment HexDigits: HexDigit ((HexDigit | '_')* HexDigit)?;
fragment HexDigit: [0-9a-fA-F];
fragment Digits: [0-9] ([0-9_]* [0-9])?;
fragment LetterOrDigit: [a-zA-Z0-9$_];
fragment Letter: [a-zA-Z$_];

// Whitespace
WS: [ \t\r\n\u000C]+ -> channel(HIDDEN);