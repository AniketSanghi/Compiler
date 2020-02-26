%{

#include "parser.tab.h"
void yyerror (char *s);
extern "C" int yylex();

#include <iostream>
#include <bits/stdc++.h>
using namespace std;

long long int wordcount = 0, Declarative = 0, Interrogative = 0, Exclamatory = 0;

string TableOfContents = "Table of Contents:\n";

string Title = "";

void clean(string &str) {

	for(int i = 1; i<str.length(); ++i) {

		if(str[i] == '\t') str[i] = ' ';
		if((str[i] == str[i-1] && str[i] == ' ') || (str[i] == ' ' && i+1<str.length() && str[i+1] == ':')) {
			str.erase(str.begin() + i);
			i--;
		}
		else if(str[i] ==':' && str[i+1] != ' ') {
			str.insert(str.begin() + i+1, ' ');
		}

	}
	str = str + '\n';
}

%}


word             [A-Za-z]+
int_float        [0-9]+\.?[0-9]*      
word_sep         [ \t,;]+
newline2         [\n][\n]+
newline          [\n]
Title            Title[^\n]*
ChapterTitle     Chapter[^\n]*
SectionTitle     Section[^\n]*

%%

{Title}          {string str(yytext, yytext + yyleng);clean(str); Title = str; return TITLE;}          
{ChapterTitle}   {string str(yytext, yytext + yyleng);clean(str); TableOfContents = TableOfContents + str; return CHAPTER_TITLE;}
{SectionTitle}   {string str(yytext, yytext + yyleng);clean(str); TableOfContents = TableOfContents + "    " + str; return SECTION_TITLE;}

{word_sep}       {if(count(yytext, yytext + yyleng, ',') > 1) {cout<<"Multiple commas detected\n"; exit(-1);} else if(count(yytext, yytext + yyleng, ';') > 1) {cout<<"Multiple semicolons detected\n";exit(-1); } else if(count(yytext, yytext + yyleng, ',') == 1 && count(yytext, yytext + yyleng, ';')) {cout<<"Comma and Semicolon both detected"; exit(-1);}}
{int_float}      {return WORD;}


{newline}        { return NEWLINE;}
{newline2}       { return NEWLINE2;}
{word}           { wordcount += 1; return WORD;}

"?"              { Interrogative += 1; return '?';}
"."              { Declarative += 1; return '.';}
"!"              { Exclamatory += 1; return '!';}

.                {cerr<<"Lexer error"; exit(-1);}
%%

int yywrap (void) {

	return 1;
}