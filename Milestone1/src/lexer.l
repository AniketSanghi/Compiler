%{

#include "parser.tab.h"
void yyerror (char *s);
extern "C" int yylex();

#include <iostream>
#include <string>

using namespace std;

int error = 0;
long long int line = 1;
	
%}

/* ------------------- */
/* Regular Definations */

delim          [ \t\f]
newline        [\n\r]
java_letter    [a-zA-Z_\$]
java_digit     [0-9]
dec            0|[1-9][0-9]*(_+[0-9]+)*
decimal        0[lL]?|[1-9][0-9]*(_+[0-9]+)*[lL]?
hex            [0-9a-fA-F]
hexadecimal    (0x|0X){hex}+(_+{hex}+)*[lL]?
binary         (0b|0B)[01]+(_+[01]+)*[lL]?
octadecimal    0(_*[0-7]+)+[lL]?
exp_int        ([eE][+-]?[0-9](_*[0-9]+)*)
whole_int      [0-9]+(_+[0-9]+)*
frac_int       \.{whole_int}
float_int      ({dec}|{whole_int}\.|{frac_int}|{whole_int}{frac_int}){exp_int}?[fFdD]?
float_int2     {whole_int}{exp_int}[fFdD]?
exp_hex        ([pP][+-]?{hex}(_*{hex}+)*)
whole_hex      {hex}+(_+{hex}+)*
frac_hex       \.{whole_hex}
float_hex      (0x|0X)({whole_hex}\.?|{frac_hex}|{whole_hex}{frac_hex}){exp_hex}[fFdD]?
escape_seq     \\([btnfr\"'\\]|[0-7][0-7]?|[0-3][0-7][0-7]|u+{hex}{hex}{hex}{hex})

/* --------------------------- */
/* Patterns for various tokens */

white_space    {delim}+
comments       \/\/.*|\/\*([^\*]*(\*+[^\*\/]+)*)*\**\*\/

identifiers    {java_letter}({java_letter}|{java_digit})*

int_literal    {decimal}|{hexadecimal}|{octadecimal}|{binary}
flt_literal    {float_int}|{float_int2}|{float_hex}
bool_literal   false|true
chr_literal    ('[^'\\\n]'|'{escape_seq}')
str_literal    \"([^\"\\\n]|{escape_seq})*\"
null_literal   null
literals       {int_literal}|{flt_literal}|{bool_literal}|{chr_literal}|{str_literal}|{null_literal}


/* -+-+-+-+-+-+-+-+-+-+-+-+-+- */
/* Lexical patterns for tokens */
/* -+-+-+-+-+-+-+-+-+-+-+-+-+- */

%%

"abstract"		  { yylval.str=strdup(yytext); return(ABSTRACT); }  
"continue"		  { yylval.str=strdup(yytext); return(CONTINUE); }
"for"             { yylval.str=strdup(yytext); return(FOR); }
"new"             { yylval.str=strdup(yytext); return(NEW); }
"switch"          { yylval.str=strdup(yytext); return(SWITCH); }
"assert"     	  { yylval.str=strdup(yytext); return(ASSERT); }
"default"    	  { yylval.str=strdup(yytext); return(DEFAULT); }
"if"			  { yylval.str=strdup(yytext); return(IF); }           
"package"     	  { yylval.str=strdup(yytext); return(PACKAGE); }
"synchronized"    { yylval.str=strdup(yytext); return(SYNCHRONIZED); }
"boolean"		  { yylval.str=strdup(yytext); return(BOOLEAN); }	
"do"              { yylval.str=strdup(yytext); return(DO); } 
"goto"            { yylval.str=strdup(yytext); return(GOTO); }
"private"     	  { yylval.str=strdup(yytext); return(PRIVATE); }
"this"			  { yylval.str=strdup(yytext); return(THIS); }
"break"      	  { yylval.str=strdup(yytext); return(BREAK); }
"double"     	  { yylval.str=strdup(yytext); return(DOUBLE); }
"implements"      { yylval.str=strdup(yytext); return(IMPLEMENTS); }
"protected"       { yylval.str=strdup(yytext); return(PROTECTED); }
"throw"    		  { yylval.str=strdup(yytext); return(THROW); }
"byte"  		  { yylval.str=strdup(yytext); return(BYTE); }
"else"			  { yylval.str=strdup(yytext); return(ELSE); }
"import"		  { yylval.str=strdup(yytext); return(IMPORT); }
"public"		  { yylval.str=strdup(yytext); return(PUBLIC); }
"throws"		  { yylval.str=strdup(yytext); return(THROWS); }
"case"			  { yylval.str=strdup(yytext); return(CASE); }
"enum"			  { yylval.str=strdup(yytext); return(ENUM); }
"instanceof"	  { yylval.str=strdup(yytext); return(INSTANCEOF); }
"return"		  { yylval.str=strdup(yytext); return(RETURN); }
"transient"		  { yylval.str=strdup(yytext); return(TRANSIENT); }
"catch"			  { yylval.str=strdup(yytext); return(CATCH); }
"extends"		  { yylval.str=strdup(yytext); return(EXTENDS); }
"int"			  { yylval.str=strdup(yytext); return(INT); }
"short"			  { yylval.str=strdup(yytext); return(SHORT); }
"try"			  { yylval.str=strdup(yytext); return(TRY); }
"char"			  { yylval.str=strdup(yytext); return(CHAR); }
"final"			  { yylval.str=strdup(yytext); return(FINAL); }
"interface"		  { yylval.str=strdup(yytext); return(INTERFACE); }
"static"		  { yylval.str=strdup(yytext); return(STATIC); }
"void"		 	  { yylval.str=strdup(yytext); return(VOID); }
"class"			  { yylval.str=strdup(yytext); return(CLASS); }
"finally"		  { yylval.str=strdup(yytext); return(FINALLY); }
"long"			  { yylval.str=strdup(yytext); return(LONG); }
"strictfp"		  { yylval.str=strdup(yytext); return(STRICTFP); }
"volatile"		  { yylval.str=strdup(yytext); return(VOLATILE); }
"const"			  { yylval.str=strdup(yytext); return(CONST); }
"float"			  { yylval.str=strdup(yytext); return(FLOAT); }	
"native"		  { yylval.str=strdup(yytext); return(NATIVE); }
"super"			  { yylval.str=strdup(yytext); return(SUPER); }
"while"			  { yylval.str=strdup(yytext); return(WHILE); }

{comments}        { for(int i = 0; i<yyleng; ++i) if(yytext[i] == '\n') line++; /* comments, meant to be ignored */}
{white_space}     { /* white spaces, meant to be ignored */}
{newline}         { line++; /* Ignores and keeps track of line number */}

{literals}        { yylval.str = strdup(yytext); return LITERAL; }
{identifiers}     { yylval.str = strdup(yytext); return IDENTIFIER; }

"("               { yylval.str=strdup(yytext); return '('; }
")"               { yylval.str=strdup(yytext); return ')'; }
"{"               { yylval.str=strdup(yytext); return '{'; }
"}"               { yylval.str=strdup(yytext); return '}'; }
"["               { yylval.str=strdup(yytext); return '['; }
"]"               { yylval.str=strdup(yytext); return ']'; }
";"               { yylval.str=strdup(yytext); return ';'; }
","               { yylval.str=strdup(yytext); return ','; }
"."               { yylval.str=strdup(yytext); return '.'; }
"..."             { yylval.str=strdup(yytext); return ELLIPSIS; }
"::"              { yylval.str=strdup(yytext); return PROPORTION; }
"="               { yylval.str=strdup(yytext); return '='; }
">"               { yylval.str=strdup(yytext); return '>'; }
"<"               { yylval.str=strdup(yytext); return '<'; }
"!"               { yylval.str=strdup(yytext); return '!'; }
"~"               { yylval.str=strdup(yytext); return '~'; }
"?"               { yylval.str=strdup(yytext); return '?'; }
":"               { yylval.str=strdup(yytext); return ':'; }
"->"              { yylval.str=strdup(yytext); return PTR_OP; }
"=="              { yylval.str=strdup(yytext); return EQ_OP; }
">="              { yylval.str=strdup(yytext); return GE_OP; }
"<="              { yylval.str=strdup(yytext); return LE_OP; }
"!="              { yylval.str=strdup(yytext); return NE_OP; }
"&&"              { yylval.str=strdup(yytext); return AND_OP; }
"||"              { yylval.str=strdup(yytext); return OR_OP; }
"++"              { yylval.str=strdup(yytext); return INC_OP; }
"--"              { yylval.str=strdup(yytext); return DEC_OP; }
"+"               { yylval.str=strdup(yytext); return '+'; }
"-"               { yylval.str=strdup(yytext); return '-'; }
"*"               { yylval.str=strdup(yytext); return '*'; }
"/"               { yylval.str=strdup(yytext); return '/'; }
"&"               { yylval.str=strdup(yytext); return '&'; }
"|"               { yylval.str=strdup(yytext); return '|'; }
"^"               { yylval.str=strdup(yytext); return '^'; }
"%"               { yylval.str=strdup(yytext); return '%'; }
"<<"              { yylval.str=strdup(yytext); return LEFT_OP; }
">>"              { yylval.str=strdup(yytext); return RIGHT_OP; }
">>>"             { yylval.str=strdup(yytext); return UNSIGNED_RIGHT_OP; }
"+="              { yylval.str=strdup(yytext); return ADD_ASSIGN; }
"-="              { yylval.str=strdup(yytext); return SUB_ASSIGN; }
"*="              { yylval.str=strdup(yytext); return MUL_ASSIGN; }
"/="              { yylval.str=strdup(yytext); return DIV_ASSIGN; }
"&="              { yylval.str=strdup(yytext); return AND_ASSIGN; }
"|="              { yylval.str=strdup(yytext); return OR_ASSIGN; }
"^="              { yylval.str=strdup(yytext); return XOR_ASSIGN; }
"%="              { yylval.str=strdup(yytext); return MOD_ASSIGN; }
"<<="             { yylval.str=strdup(yytext); return LEFT_ASSIGN; }
">>="             { yylval.str=strdup(yytext); return RIGHT_ASSIGN; }
">>>="            { yylval.str=strdup(yytext); return UNSIGNED_RIGHT_ASSIGN; }

.                 { cerr<<line<<": error :: Illegal Character >>> "<<yytext<<"\n"; exit(-1); }

%%


int yywrap(void){ return 1;} 
