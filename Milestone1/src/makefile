parser: lex.yy.c parser.tab.c
	g++ -std=c++11 lex.yy.c parser.tab.c -o parser

lex.yy.c: parser.tab.c lexer.l
	flex lexer.l

parser.tab.c: parser.y
	bison -d parser.y

clean: 
	rm -rf lex.yy.c parser.tab.c parser.tab.h parser parser.dSYM