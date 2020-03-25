import java.io.*;

class Symbol {

	public String content;
	public TokenType tokenType;
	public int yyline, yycolumn;

	public Symbol( TokenType tokenType, int yyline, int yycolumn, String content ) {
		this.content = content;
		this.tokenType = tokenType;
		this.yyline = yyline;
		this.yycolumn = yycolumn;
	}

}

enum TokenType {

	INT, IF, SHORT, ELSE, LONG, SWITCH, FLOAT,
	CASE, DOUBLE, DEFAULT, CHAR, AUTO, STRING,
	VOLATILE, CONST, STATIC, FOR, GOTO, FOREACH,
	SIGNED, WHILE, BOOL, DO, VOID, IN, RETURN,
	BREAK, RECORD, CONTINUE, NEW, UNTIL, SIZEOF, FUNCTION,
     PRINTLN, TRUE, FALSE,
	LESSTHAN, MORETHAN,
	IDENTIFIER, OTHER, INTEGERLITERAL,
	FLOATLITERAL, ENTER, TAB,
	SPECIAL_CHARACTER,
	COMMENT, NORMAL_CHARACTER,
	NOTHING, EOF

}

%%

%public
%line
%column

%class MyScanner
%unicode

%type Symbol

%function next

%state STRING, CHARACTER

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

TraditionalComment = "/*"~"*/"
InLineComment = "//" {InputCharacter}* {LineTerminator}

Comment = {TraditionalComment}|{InLineComment}

Letter = [A-Za-z]
Digit = [0-9]
Underscore = "_"

Identifier = {Letter} ({Letter}|{Digit}|{Underscore})*

Zero = 0
Octal = 0[0-7]+
Decimal = [1-9][0-9]*
HexaDecimal = [0][xX][0-9a-fA-F]+;

IntegerNumbers = {Zero}|{Octal}|{Decimal}|{HexaDecimal}

NormalFloat = ( ({Digit}+\.{Digit}*) | ({Digit}*\.{Digit}+) )
ScientificFloat = ( {NormalFloat}|{Zero}|{Decimal} ) "e" ( \+|\- )? {Digit}+

FloatNumbers = NormalFloat | ScientificFloat

StringCharacter = [^\n\r\t\v\b\f\a\?\0\\]
SingleCharacter = [^\n\r\t\v\b\f\a\?\0\\]

NormalCharacter = "'" {SingleCharacter} "'"
Other = "==" | "." | "!=" | "," |"<=" |":" |"<" |";" |">" |  "[" | "]" |  ">=" | "++" | "=" | "--" | "~" | "-" |"&" |"-=" |"and |"*=" |"or" |"/=" |"not" |"/" |"|" |"%" |"^" |"begin" | "end" | "*" | "(" | ")" | "+" | "+="
%%

<YYINITIAL> {

	/* keywords */
	"int"          { return new Symbol( TokenType.INT, yyline, yycolumn, "int" ); }
	"if"           { return new Symbol( TokenType.IF, yyline, yycolumn, "if" ); }
	"short"        { return new Symbol( TokenType.SHORT, yyline, yycolumn, "short" ); }
	"else"         { return new Symbol( TokenType.ELSE, yyline, yycolumn, "else" ); }
	"long"         { return new Symbol( TokenType.LONG, yyline, yycolumn, "long" ); }
	"switch"         { return new Symbol( TokenType.SWITCH, yyline, yycolumn, "switch" ); }
	"float"      { return new Symbol( TokenType.FLOAT, yyline, yycolumn, "float" ); }
	"case"         { return new Symbol( TokenType.CASE, yyline, yycolumn, "case" ); }
	"double"          { return new Symbol( TokenType.DOUBLE, yyline, yycolumn, "double" ); }
	"default"          { return new Symbol( TokenType.DEFAULT, yyline, yycolumn, "default" ); }
	"char"        { return new Symbol( TokenType.CHAR, yyline, yycolumn, "char" ); }
	"auto"      { return new Symbol( TokenType.AUTO, yyline, yycolumn, "auto" ); }
	"string"           { return new Symbol( TokenType.STRING, yyline, yycolumn, "string" ); }
	"volatile"        { return new Symbol( TokenType.VOLATILE, yyline, yycolumn, "volatile" ); }
	"const"          { return new Symbol( TokenType.CONST, yyline, yycolumn, "const" ); }
	"static"          { return new Symbol( TokenType.STATIC, yyline, yycolumn, "static" ); }
	"for"          { return new Symbol( TokenType.FOR, yyline, yycolumn, "for" ); }
	"goto"      { return new Symbol( TokenType.GOTO, yyline, yycolumn, "goto" ); }
	"foreach"       { return new Symbol( TokenType.FOREACH, yyline, yycolumn, "foreach" ); }
	"signed"       { return new Symbol( TokenType.SIGNED, yyline, yycolumn, "signed" ); }
	"while"          { return new Symbol( TokenType.WHILE, yyline, yycolumn, "while" ); }
	"bool"        { return new Symbol( TokenType.BOOL, yyline, yycolumn, "bool" ); }
	"do"      { return new Symbol( TokenType.DO, yyline, yycolumn, "do" ); }
	"void"          { return new Symbol( TokenType.VOID, yyline, yycolumn, "void" ); }
	"in"        { return new Symbol( TokenType.IN, yyline, yycolumn, "in" ); }
	"return"        { return new Symbol( TokenType.RETURN, yyline, yycolumn, "return" ); }
	"break"         { return new Symbol( TokenType.BREAK, yyline, yycolumn, "break" ); }
	"record"            { return new Symbol( TokenType.RECORD, yyline, yycolumn, "record" ); }
	"continue"            { return new Symbol( TokenType.CONTINUE, yyline, yycolumn, "continue" ); }
	"new"        { return new Symbol( TokenType.NEW, yyline, yycolumn, "new" ); }
	"until"         { return new Symbol( TokenType.UNTIL, yyline, yycolumn, "until" ); }
	"sizeof"         { return new Symbol( TokenType.SIZEOF, yyline, yycolumn, "sizeof" ); }
	"function"         { return new Symbol( TokenType.FUNCTION, yyline, yycolumn, "function" ); }
	"do"         { return new Symbol( TokenType.DO, yyline, yycolumn, "do" ); }
	"println"         { return new Symbol( TokenType.PRINTLN, yyline, yycolumn, "println" ); }
	"true"         { return new Symbol( TokenType.TRUE, yyline, yycolumn, "true" ); }
	"false"         { return new Symbol( TokenType.FALSE, yyline, yycolumn, "false" ); }
     "<"             { return new Symbol( TokenType.LESSTHAN, yyline, yycolumn, "lessthan" ); }
 	">"             { return new Symbol( TokenType.MORETHAN, yyline, yycolumn, "morethan" ); }

	{Identifier}    { return new Symbol( TokenType.IDENTIFIER, yyline, yycolumn,yytext()  ); }
    {Other}          { return new Symbol( TokenType.OTHER, yyline, yycolumn,yytext() ); }
	{IntegerNumbers}    { return new Symbol( TokenType.INTEGERLITERAL, yyline, yycolumn, yytext() ); }

	{NormalFloat}  { return new Symbol( TokenType.FLOATLITERAL, yyline, yycolumn, yytext() ); }
	{ScientificFloat}   { return new Symbol( TokenType.FLOATLITERAL, yyline, yycolumn, yytext() ); }

	{Comment}       { return new Symbol( TokenType.COMMENT, yyline, yycolumn, yytext() ); }

	"\t"           { return new Symbol( TokenType.TAB, yyline, yycolumn, "\t" ); }

	"\""              { yybegin( STRING ); return new Symbol( TokenType.STRING, yyline, yycolumn, yytext() ); }

	//{NormalCharacter}   { return new Symbol( TokenType.NORMAL_CHARACTER, yyline, yycolumn, yytext() ); }
	"'"             { yybegin( CHARACTER ); return new Symbol( TokenType.NORMAL_CHARACTER, yyline, yycolumn, "'" ); }

	{LineTerminator}    {return new Symbol( TokenType.ENTER, yyline, yycolumn, "\n" ); }

	[^]             { return new Symbol( TokenType.NOTHING, yyline, yycolumn, yytext() ); }

	//<<EOF>>

}

<STRING> {

	"\""          { yybegin( YYINITIAL ); return new Symbol( TokenType.STRING, yyline, yycolumn, yytext() ); }

//	{StringCharacter}+      { return new Symbol( TokenType.STRING, yyline, yycolumn, yytext() );  }

	"\\n"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\n" ); }
	"\\t"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\t" ); }
	"\\v"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\v" ); }
	"\\b"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\b" ); }
	"\\r"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\r" ); }
	"\\f"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\f" ); }
	"\\a"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\a" ); }
	"\\\\"      { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\\\" ); }
	"\\?"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\?" ); }
	"\\'"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\'" ); }
	"\\\""      { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\\"" ); }
	"\\0"       { return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\0" ); }

	.           { return new Symbol( TokenType.STRING, yyline, yycolumn, yytext() ); }


}

<CHARACTER> {

	"\\n"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\n'" ); }
	"\\t"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\t'" ); }
	"\\v"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\v'" ); }
	"\\b"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\b'" ); }
	"\\r"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\r'" ); }
	"\\f"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\f'" ); }
	"\\a"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\a'" ); }
	"\\\\"\'    { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\\\'" ); }
	"\\?"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\?'" ); }
	"\\'"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\''" ); }
	"\\\""\'    { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\\"'" ); }
	"\\0"\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.SPECIAL_CHARACTER, yyline, yycolumn, "\\0'" ); }

	{SingleCharacter}\'     { yybegin( YYINITIAL ); return new Symbol( TokenType.NORMAL_CHARACTER, yyline, yycolumn, yytext() ); }


}

<<EOF>>             { return new Symbol( TokenType.EOF, yyline, yycolumn, "EOF" ); }