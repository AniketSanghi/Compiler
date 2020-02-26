%{

#include <iostream>
using namespace std;

void yyerror (char *s);
extern "C" int yylex();

extern long long int wordcount, Declarative, Interrogative, Exclamatory;
extern string TableOfContents, Title;

long long int chaptercount = 0, sectioncount = 0, paragraphcount = 0, sentencecount = 0;

%}


%right  TITLE CHAPTER_TITLE SECTION_TITLE WORD NEWLINE '?' '.' '!' NEWLINE2

%%

Dissertation
    : TITLE NEWLINE Chapters
    | TITLE NEWLINE2 Chapters
    ;

Chapters
    : Chapter             {chaptercount += 1;}
    | Chapters Chapter    {chaptercount += 1;}
    ;

Chapter
    : CHAPTER_TITLE NEWLINE Paragraphs
    | CHAPTER_TITLE NEWLINE Paragraphs Sections
    | CHAPTER_TITLE NEWLINE Sections
    | CHAPTER_TITLE NEWLINE2 Paragraphs
    | CHAPTER_TITLE NEWLINE2 Paragraphs Sections
    | CHAPTER_TITLE NEWLINE2 Sections
    ;

Sections
    : Section               {sectioncount += 1;}
    | Sections Section      {sectioncount += 1;}
    ;

Section
    : SECTION_TITLE NEWLINE Paragraphs
    | SECTION_TITLE NEWLINE2 Paragraphs
    ;

Paragraphs
    : Paragraph                      {paragraphcount += 1;}
    | Paragraph NEWLINE2              {paragraphcount += 1;}
    | Paragraphs Paragraph           {paragraphcount += 1;}
    | Paragraphs Paragraph NEWLINE2   {paragraphcount += 1;}
    ;

Paragraph
    : Sentence              
    | Paragraph Sentence    
    ;

Sentence
    : Line '?'    {sentencecount += 1;}
    | Line '!'    {sentencecount += 1;}
    | Line '.'    {sentencecount += 1;}
    ;

Line
    : WORD
    | Line WORD
    ;

%%                    


int main (void) {

	if(yyparse () != 0) return 1;

	cout<<Title;
	cout<<"Number of Chapters: "<<chaptercount<<"\n";
	cout<<"Number of Sections: "<<sectioncount<<"\n";
	cout<<"Number of Paragraphs: "<<paragraphcount<<"\n";
	cout<<"Number of Sentences: "<<sentencecount<<"\n";
	cout<<"Number of Words: "<<wordcount<<"\n";
	cout<<"Number of Declarative Sentences: "<<Declarative<<"\n";
	cout<<"Number of Exclamatory Sentences: "<<Exclamatory<<"\n";
	cout<<"Number of Interrogative Sentences: "<<Interrogative<<"\n";
	cout<<TableOfContents;
}

void yyerror (char *s) {

	cerr<<s;
} 
