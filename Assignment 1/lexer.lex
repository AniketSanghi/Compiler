/* Global definitions */
/* Importing c++ libraries for help in maintaining counts */

%{

#include <iostream>
#include <unordered_map>
#include <string>
#include <bits/stdc++.h>

using namespace std;

unordered_map <string, pair <string, long long int> > data_map;

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

key1           abstract|continue|for|new|switch|assert|default|package
key2           if|synchronized|boolean|do|goto|private|this|break|double
key3           implements|protected|throw|byte|else|import|public|throws
key4           case|enum|instanceof|return|transient|catch|extends|int
key5           short|try|char|final|interface|static|void|class|finally
key6           long|strictfp|volatile|float|native|super|while|const
keywords       {key1}|{key2}|{key3}|{key4}|{key5}|{key6}

identifiers    {java_letter}({java_letter}|{java_digit})*

int_literal    {decimal}|{hexadecimal}|{octadecimal}|{binary}
flt_literal    {float_int}|{float_int2}|{float_hex}
bool_literal   false|true
chr_literal    ('[^'\\\n]'|'{escape_seq}')
str_literal    \"([^\"\\\n]|{escape_seq})*\"
null_literal   null
literals       {int_literal}|{flt_literal}|{bool_literal}|{chr_literal}|{str_literal}|{null_literal}

separator     ([\(\){}\[\];,\.@]|\.\.\.|::)

operator      (([\+\-\*\/&\|\^%=<>!]|<<|>>|>>>)[=]?|&&|\|\||\+\+|\-\-|\->|:|\?|~)


/* -+-+-+-+-+-+-+-+-+-+-+-+-+- */
/* Lexical patterns for tokens */
/* -+-+-+-+-+-+-+-+-+-+-+-+-+- */

%%

{keywords}    { data_map[yytext].first = "Keyword"; data_map[yytext].second += 1; }
{comments}    { line += count(yytext, yytext + yyleng, '\n');/* comments, meant to be ignored */}
{white_space} { /* white spaces, meant to be ignored */}
{newline}     { line++; /* Ignores and keeps track of line number */}
{literals}    { data_map[yytext].first = "Literal"; data_map[yytext].second += 1; }
{identifiers} { data_map[yytext].first = "Identifier"; data_map[yytext].second += 1; }
{separator}   { data_map[yytext].first = "Separator"; data_map[yytext].second += 1; }
{operator}    { data_map[yytext].first = "Operator"; data_map[yytext].second += 1; }
.             { cerr<<line<<": error :: Illegal Character >>> "<<yytext<<"\n"; error = 1; }

%%



int yywrap(){} 
int main() {
	
	yylex();

	if(error) return 1;

	unordered_map <string, pair <string, long long int> > :: iterator it = data_map.begin();
	cout<<"Lexeme,Token,Count\n";
	while(it != data_map.end()) {
		cout<<it->first<<","<<it->second.first<<","<<it->second.second<<"\n";
		it++;
	}
	return 0;
}