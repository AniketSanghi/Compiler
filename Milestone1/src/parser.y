%{

#include <iostream>
#include <string>
#include <vector>
using namespace std;

#define YYERROR_VERBOSE

void yyerror (const char *s);
extern "C" int yylex();
extern long long int line;


struct Node {

    string label, value, e_label;
    vector< Node* > children;
};

Node *root = NULL;

Node *createNode(string label, string value, vector <Node *> children) {

    Node *newNode = new Node;
    newNode->label = label;
    newNode->value = value;
    newNode->children = children;
    newNode->e_label = "";
    return newNode;
}

%}

// %locations
// %define api.pure full
// %define parse.error verbose
// %define parse.lac full
// %error-verbose

%union{
    char *str;
    struct Node *node;
}

%start CompilationUnit

%token <str> ABSTRACT CONTINUE FOR SWITCH
%token <str> ASSERT DEFAULT IF PACKAGE SYNCHRONIZED
%token <str> BOOLEAN DO GOTO PRIVATE THIS
%token <str> BREAK DOUBLE IMPLEMENTS PROTECTED THROW
%token <str> BYTE ELSE IMPORT PUBLIC THROWS
%token <str> CASE ENUM RETURN TRANSIENT
%token <str> CATCH EXTENDS INT SHORT TRY
%token <str> CHAR FINAL INTERFACE STATIC VOID
%token <str> CLASS FINALLY LONG STRICTFP VOLATILE
%token <str> CONST FLOAT NATIVE SUPER WHILE

%token <str> LITERAL IDENTIFIER


%right <str> '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN UNSIGNED_RIGHT_ASSIGN
%right <str> '?' ':'
%left  <str> OR_OP
%left  <str> AND_OP
%left  <str> '|'
%left  <str> '^'
%left  <str> '&'
%left  <str> NE_OP EQ_OP
%nonassoc <str> '<' '>' GE_OP LE_OP INSTANCEOF
%left  <str> LEFT_OP RIGHT_OP UNSIGNED_RIGHT_OP
%left  <str> '+' '-'
%left  <str> '*' '/' '%'
%right <str> NEW
%right <str> INC_OP DEC_OP '!' '~'

%right <str> '(' ')' '{' '}' '[' ']' '.' ',' ';' ELLIPSIS PROPORTION PTR_OP

%type <node> CompilationUnit PackageDeclaration ImportDeclaration SingleTypeImportDeclaration
%type <node> TypeImportOnDemandDeclaration SingleStaticImportDeclaration StaticImportOnDemandDeclaration
%type <node> TypeDeclaration TypeName Type PrimitiveType NumericType IntegralType FloatingPointType
%type <node> ReferenceType ArrayType Dims ArrayInitializer VariableInitializerList ClassDeclaration
%type <node> NormalClassDeclaration ClassModifier Superclass Superinterfaces InterfaceTypeList
%type <node> ClassBody ClassMemberDeclaration ClassBodyDeclaration FieldDeclaration VariableDeclaratorList
%type <node> VariableDeclarator VariableDeclaratorId VariableInitializer MethodDeclaration MethodHeader
%type <node> MethodDeclarator FormalParameterList FormalParameters FormalParameter LastFormalParameter
%type <node> ReceiverParameter Throws ExceptionTypeList ExceptionType MethodBody InstanceInitializer
%type <node> StaticInitializer ConstructorDeclaration ConstructorDeclarator ConstructorBody 
%type <node> ExplicitConstructorInvocation EnumDeclaration EnumBody EnumConstantList EnumConstant EnumBodyDeclarations
%type <node> InterfaceDeclaration NormalInterfaceDeclaration ExtendsInterfaces InterfaceBody
%type <node> InterfaceMemberDeclaration ConstantDeclaration InterfaceMethodDeclaration Block
%type <node> BlockStatements BlockStatement LocalVariableDeclarationStatement LocalVariableDeclaration
%type <node> Statement StatementNoShortIf StatementWithoutTrailingSubstatement EmptyStatement LabeledStatement
%type <node> LabeledStatementNoShortIf ExpressionStatement StatementExpression IfThenStatement IfThenElseStatement
%type <node> IfThenElseStatementNoShortIf AssertStatement SwitchStatement SwitchBlock SwitchBlockStatementGroup
%type <node> SwitchLabel WhileStatement WhileStatementNoShortIf DoStatement ForStatement ForStatementNoShortIf
%type <node> BasicForStatement SINGLE_ForInit SINGLE_Expression SINGLE_ForUpdate BasicForStatementNoShortIf ForInit 
%type <node> ForUpdate StatementExpressionList EnhancedForStatement EnhancedForStatementNoShortIf BreakStatement
%type <node> ContinueStatement ReturnStatement ThrowStatement SynchronizedStatement TryStatement 
%type <node> Catches CatchClause CatchFormalParameter CatchType Finally TryWithResourcesStatement 
%type <node> ResourceSpecification ResourceList Resource 
%type <node> Primary PrimaryNoNewArray ClassLiteral ClassInstanceCreationExpression
%type <node> UnqualifiedClassInstanceCreationExpression FieldAccess ArrayAccess
%type <node> MethodInvocation ArgumentList MethodReference ArrayCreationExpression
%type <node> DimExprs DimExpr Expression LambdaExpression LambdaParameters
%type <node> LambdaBody AssignmentExpression Assignment LeftHandSide
%type <node> AssignmentOperator ConditionalExpression ConditionalOrExpression
%type <node> ConditionalAndExpression InclusiveOrExpression ExclusiveOrExpression
%type <node> AndExpression EqualityExpression RelationalExpression ShiftExpression
%type <node> AdditiveExpression MultiplicativeExpression UnaryExpression PreIncrementExpression
%type <node> PreDecrementExpression UnaryExpressionNotPlusMinus PostfixExpression
%type <node> PostIncrementExpression PostDecrementExpression CastExpression
%type <node> MULTI_ImportDeclaration MULTI_TypeDeclaration
%type <node> MULTI_ClassModifier MULTI_ClassBodyDeclaration SINGLE_ArgumentList
%type <node> MULTI_InterfaceMemberDeclaration MULTI_SwitchBlockStatementGroup MULTI_SwitchLabel

%%



/* PRODUCTION #7 (Packages) ----------------------------------------------- */

CompilationUnit
    : PackageDeclaration MULTI_ImportDeclaration MULTI_TypeDeclaration   { root = createNode("PROGRAM", "", {$1, $2, $3}); }
    | MULTI_ImportDeclaration MULTI_TypeDeclaration                      { root = createNode("PROGRAM", "", {$1, $2}); }
    | PackageDeclaration MULTI_TypeDeclaration                           { root = createNode("PROGRAM", "", {$1, $2}); }
    | MULTI_TypeDeclaration                                              { root = createNode("PROGRAM", "", {$1}); }
    | PackageDeclaration MULTI_ImportDeclaration                         { root = createNode("PROGRAM", "", {$1, $2});}
    | MULTI_ImportDeclaration                                            { root = createNode("PROGRAM", "", {$1});}
    | PackageDeclaration                                                 { root = createNode("PROGRAM", "", {$1});}
    | /* Empty */                                                        { root = createNode("PROGRAM", "", {});  }
    ; 

PackageDeclaration
    : PACKAGE TypeName ';'                       { $$ = createNode("Package", $1 , {$2}); }
    ;

ImportDeclaration
    : SingleTypeImportDeclaration                { $$ = $1; }
    | TypeImportOnDemandDeclaration              { $$ = $1; }  
    | SingleStaticImportDeclaration              { $$ = $1; }
    | StaticImportOnDemandDeclaration            { $$ = $1; }
    ;

SingleTypeImportDeclaration
    : IMPORT TypeName ';'                        { $$ = createNode("Import", $1 , {$2}); }
    ;

TypeImportOnDemandDeclaration
    : IMPORT TypeName '.' '*' ';'                { Node *temp = createNode("OP", $4, {} ); $$ = createNode("Import", $1 , {$2, temp}); }
    ;

SingleStaticImportDeclaration
    : IMPORT STATIC TypeName '.' IDENTIFIER ';'  { Node* lexeme = createNode("ID", $5, {}); $3->children.push_back(lexeme); $$ = createNode("Import_static", $1 , {$3}); }
    ;

StaticImportOnDemandDeclaration
    : IMPORT STATIC TypeName '.' '*' ';'         { Node *temp = createNode("OP", $5, {} ); $$ = createNode("Import_Static", $1 , {$3, temp}); }
    ;

TypeDeclaration
    : ClassDeclaration                           { $$ = $1; }
    | InterfaceDeclaration                       { $$ = $1; }
    | ';'                                        { $$ = createNode("EMP", $1, {}); }
    ;







/* PRODUCTION #6 (Names) ----------------------------------------------- */

TypeName
    : IDENTIFIER                { Node* lexeme = createNode("ID", $1, {}); $$ = createNode("Name","", {lexeme}); }
    | TypeName '.' IDENTIFIER   { Node* lexeme = createNode("ID", $3, {}); $1->children.push_back(lexeme); $$ = $1; }
    ;




/* PRODUCTION #4 (Types, Values and Variables) ----------------------------------------------- */

Type
    : PrimitiveType             { $$ = $1; }
    | ReferenceType             { $$ = $1; /*$$->label = "Classtype"; */}
    ;

PrimitiveType
    : NumericType               { $$ = $1; }
    | BOOLEAN                   { $$ = createNode("TYPE", $1, {} ); }
    ;

NumericType
    : IntegralType              { $$ = $1; }
    | FloatingPointType         { $$ = $1; }
    ;

IntegralType
    : BYTE                      { $$ = createNode("TYPE", $1, {} ); }
    | SHORT                     { $$ = createNode("TYPE", $1, {} ); }
    | INT                       { $$ = createNode("TYPE", $1, {} ); }
    | LONG                      { $$ = createNode("TYPE", $1, {} ); }
    | CHAR                      { $$ = createNode("TYPE", $1, {} ); }
    ;

FloatingPointType
    : FLOAT                     { $$ = createNode("TYPE", $1, {} ); }
    | DOUBLE                    { $$ = createNode("TYPE", $1, {} ); }
    ;

ReferenceType
    : TypeName                  { $$ = $1; $$->label = "Classtype"; }
    | ArrayType                 { $$ = $1; }
    ;

ArrayType
    : PrimitiveType Dims                    { $$ = createNode("Array","",{$1,$2});}
    | /*treat as class type*/ TypeName Dims { $$ = createNode("Array","",{$1,$2});
                                              $1->label = "ClassType";}
    ;

Dims
    : Dims '[' ']'              { $$ = $1;Node *temp = createNode("","[]",{}); $$->children.push_back(temp);}
    | '[' ']'                   { Node *temp = createNode("","[]",{});$$ = createNode("DIMS","",{temp}); } 
    ;



/* PRODUCTION #10 (Arrays) ----------------------------------------------- */

ArrayInitializer
    : '{' VariableInitializerList ',' '}'                  { $$ = $2; }
    | '{' VariableInitializerList '}'                      { $$ = $2; }
    | '{' ',' '}'                                          { $$ = createNode("EMP", "", {}); }
    | '{' '}'                                              { $$ = createNode("EMP", "", {}); }
    ;

VariableInitializerList
    : VariableInitializer                                  { $$ = createNode("ArrayInitializer", "", {$1}); }  
    | VariableInitializerList ',' VariableInitializer      { $$ = $1; $$->children.push_back($3); }
    ;







/* PRODUCTION #8 (Classes) ----------------------------------------------- */

ClassDeclaration
    : NormalClassDeclaration         { $$ = $1; }
    | EnumDeclaration                { $$ = $1; }
    ;

NormalClassDeclaration
    : MULTI_ClassModifier CLASS IDENTIFIER Superclass Superinterfaces ClassBody  { $$ = createNode("CLASS", $3, {$1, $4, $5, $6}); }
    | CLASS IDENTIFIER Superclass Superinterfaces ClassBody                      { $$ = createNode("CLASS", $2, {$3, $4, $5}); }
    | MULTI_ClassModifier CLASS IDENTIFIER Superinterfaces ClassBody             { $$ = createNode("CLASS", $3, {$1, $4, $5}); }
    | CLASS IDENTIFIER Superinterfaces ClassBody                                 { $$ = createNode("CLASS", $2, {$3, $4}); }
    | MULTI_ClassModifier CLASS IDENTIFIER Superclass ClassBody                  { $$ = createNode("CLASS", $3, {$1, $4, $5}); }
    | CLASS IDENTIFIER Superclass ClassBody                                      { $$ = createNode("CLASS", $2, {$3, $4}); }
    | MULTI_ClassModifier CLASS IDENTIFIER ClassBody                             { $$ = createNode("CLASS", $3, {$1, $4}); }
    | CLASS IDENTIFIER ClassBody                                                 { $$ = createNode("CLASS", $2, {$3}); }
    ;

ClassModifier
    : PUBLIC         { $$ = createNode("MODIFIER", $1, {}); }
    | PROTECTED      { $$ = createNode("MODIFIER", $1, {}); }
    | PRIVATE        { $$ = createNode("MODIFIER", $1, {}); }
    | ABSTRACT       { $$ = createNode("MODIFIER", $1, {}); }
    | STRICTFP       { $$ = createNode("MODIFIER", $1, {}); }
    | STATIC         { $$ = createNode("MODIFIER", $1, {}); }
    | FINAL          { $$ = createNode("MODIFIER", $1, {}); }
    | TRANSIENT      { $$ = createNode("MODIFIER", $1, {}); }
    | VOLATILE       { $$ = createNode("MODIFIER", $1, {}); }
    | SYNCHRONIZED   { $$ = createNode("MODIFIER", $1, {}); }
    | NATIVE         { $$ = createNode("MODIFIER", $1, {}); }
    ;

Superclass
    : EXTENDS TypeName                    { $2->label = "Classtype"; $$ = createNode("Superclass", "", {$2}); }
    ;

Superinterfaces
    : IMPLEMENTS InterfaceTypeList        { $$ = $2; }
    ;

InterfaceTypeList       
    : InterfaceTypeList ',' TypeName      { $3->label = "Interfacetype"; $$->children.push_back($3); }
    | TypeName                            { $1->label = "Interfacetype"; $$ = createNode("InterfaceTypeList", "", {$1}); }
    ;

ClassBody
    : '{' MULTI_ClassBodyDeclaration '}'  { $$ = $2; }
    | '{' '}'                             { $$ = createNode("EMPTY", "", {}); }
    ;

ClassMemberDeclaration
    : FieldDeclaration                    { $$ = $1; }
    | MethodDeclaration                   { $$ = $1; }
    | ClassDeclaration                    { $$ = $1; }
    | InterfaceDeclaration                { $$ = $1; }
    | ';'                                 { $$ = createNode("EMP", "", {}); }
    ;

ClassBodyDeclaration
    : ClassMemberDeclaration              { $$ = $1; }
    | InstanceInitializer                 { $$ = $1; }
    | StaticInitializer                   { $$ = $1; }
    | ConstructorDeclaration              { $$ = $1; }
    ;

FieldDeclaration
    : MULTI_ClassModifier Type VariableDeclaratorList ';'   { $$ = createNode("FieldDeclaration", "", {$1, $2, $3}); }
    | Type VariableDeclaratorList ';'                       { $$ = createNode("FieldDeclaration", "", {$1, $2}); }
    ;

VariableDeclaratorList
    : VariableDeclaratorList ',' VariableDeclarator         { $$ = $1; $$->children.push_back($3); }
    | VariableDeclarator                                    { $$ = createNode("Variables", "", {$1}); }
    ;

VariableDeclarator
    : VariableDeclaratorId '=' VariableInitializer          { $$ = createNode("Initialisation", $2, {$1, $3});}
    | VariableDeclaratorId                                  { $$ = $1; }
    ;

VariableDeclaratorId
    : IDENTIFIER Dims                                 { Node *temp = createNode("ID",$1,{});
                                                        $$ = createNode("Variable_declarator","",{temp,$2});}
    | IDENTIFIER                                      { $$ = createNode("ID", $1, {}); }
    ;

VariableInitializer
    : Expression                                      { $$ = $1; }
    | ArrayInitializer                                { $$ = $1; }
    ;

MethodDeclaration
    : MULTI_ClassModifier MethodHeader MethodBody     {$$ = $2; $2->children.insert($2->children.begin(), $1); $$->children.push_back($3); }     
    | MethodHeader MethodBody                         {$$ = $1; $$->children.push_back($2); }
    ;

MethodHeader
    : Type MethodDeclarator Throws                    { $$ = $2; $2->children.insert($2->children.begin(), $1); $2->children.push_back($3); }
    | Type MethodDeclarator                           { $$ = $2; $2->children.insert($2->children.begin(), $1); }
    | VOID MethodDeclarator Throws                    { $$ = $2; Node *temp = createNode("TYPE", $1, {}); $2->children.insert($2->children.begin(), temp); $2->children.push_back($3);}
    | VOID MethodDeclarator                           { $$ = $2; Node *temp = createNode("TYPE", $1, {}); $2->children.insert($2->children.begin(), temp); }
    ;

MethodDeclarator
    : IDENTIFIER '(' FormalParameterList ')'         { $$ = createNode("Function", $1, {$3}); }
    | IDENTIFIER '(' ')'                             { $$ = createNode("Function", $1, {}); }
    | IDENTIFIER '(' FormalParameterList ')' Dims    { $$ = createNode("Function", $1, {$3,$5}); }
    | IDENTIFIER '(' ')' Dims                        { $$ = createNode("Function", $1, {$4}); }
    ;

FormalParameterList
    : ReceiverParameter                              { $$ = createNode("Parameters", "", {$1}); }
    | FormalParameters ',' LastFormalParameter       { $$ = $1; $1->children.push_back($3); }
    | LastFormalParameter                            { $$ = createNode("Parameters", "", {$1}); }
    ;

FormalParameters
    : FormalParameters ',' FormalParameter           { $$ = $1; $$->children.push_back($3); }
    | FormalParameter                                { $$ = createNode("Parameters", "", {$1}); }
    | ReceiverParameter                              { $$ = createNode("Parameters", "", {$1}); }
    ;

FormalParameter
    : FINAL Type VariableDeclaratorId               { Node *temp = createNode("MODIFIER", $1, {}); $$ = createNode("Parameter", "", {temp, $2, $3}); }
    | Type VariableDeclaratorId                     { $$ = createNode("Parameter", "", {$1, $2}); }
    ;

LastFormalParameter
    : FINAL Type ELLIPSIS VariableDeclaratorId      { Node *n1 = createNode("MODIFIER", $1, {}); Node *n2 = createNode("ELLIPSIS", $3, {}); $$ = createNode("LastParameter", "", {n1, $2, n2, $4}); }
    | Type ELLIPSIS VariableDeclaratorId            { Node *n2 = createNode("ELLIPSIS", $2, {}); $$ = createNode("LastParameter", "", {$1, n2, $3});}
    | FormalParameter                               { $$ = $1; }
    ;

ReceiverParameter
    : Type THIS                                     { Node *n1 = createNode("THIS", $2, {}); $$ = createNode("ReceiverParameter", "", {$1, n1}); }
    | Type IDENTIFIER '.' THIS                      { Node *n1 = createNode("THIS", $4, {}); Node *n2 = createNode("ID", $2, {}); $$ = createNode("ReceiverParameter", "", {$1, n2, n1}); }
    ;

Throws
    : THROWS ExceptionTypeList                      { $$ = createNode("THROWS", $1, {$2}); }
    ;

ExceptionTypeList
    : ExceptionTypeList ',' ExceptionType           { $$ = $1; $$->children.push_back($3); }
    | ExceptionType                                 { $$ = createNode("Exceptions", "", {$1}); }
    ;

ExceptionType
    : TypeName                                      { $$ = $1; $$->label = "Classtype"; }
    ;

MethodBody
    : Block                                         {$$ = $1; }
    | ';'                                           {$$ = createNode("EMP", "", {}); }
    ;

InstanceInitializer
    : Block                                         { $$ = $1; }
    ;

StaticInitializer
    : STATIC Block                                  { $$ = createNode("STATIC", $1, {$2}); }
    ;

ConstructorDeclaration
    : MULTI_ClassModifier ConstructorDeclarator Throws ConstructorBody     { $$ = $2; $$->children.insert($$->children.begin(), $1); $$->children.push_back($3); $$->children.push_back($4); }
    | ConstructorDeclarator Throws ConstructorBody                         { $$ = $1; $$->children.push_back($2); $$->children.push_back($3); }
    | MULTI_ClassModifier ConstructorDeclarator ConstructorBody            { $$ = $2; $$->children.insert($$->children.begin(), $1); $$->children.push_back($3);  }
    | ConstructorDeclarator ConstructorBody                                { $$ = $1; $$->children.push_back($2); }
    ;

ConstructorDeclarator
    : IDENTIFIER '(' FormalParameterList ')'                               { $$ = createNode("Constructor", $1, {$3}); }
    | IDENTIFIER '(' ')'                                                   { $$ = createNode("Constructor", $1, {}); }
    ;

ConstructorBody
    : '{' ExplicitConstructorInvocation BlockStatements '}'                { $$ = createNode("Body", "", {$2, $3}); }
    | '{' BlockStatements '}'                                              { $$ = createNode("Body", "", {$2}); }
    | '{' ExplicitConstructorInvocation '}'                                { $$ = createNode("Body", "", {$2}); }
    | '{' '}'                                                              { $$ = createNode("EMP", "", {}); }
    ;

ExplicitConstructorInvocation
    : THIS SINGLE_ArgumentList                                             { $$ = createNode("THIS", $1, {$2}); }
    | SUPER SINGLE_ArgumentList                                            { $$ = createNode("SUPER", $1, {$2}); }
    | TypeName '.' SUPER SINGLE_ArgumentList                               { $$ = createNode("SUPER", $3, {$1, $4}); }
    | Primary '.' SUPER SINGLE_ArgumentList                                { $$ = createNode("SUPER", $3, {$1, $4}); }
    ;

EnumDeclaration
    : MULTI_ClassModifier ENUM IDENTIFIER Superinterfaces EnumBody        { $$ = createNode("ENUM", $3, {$1, $4, $5}); }
    | ENUM IDENTIFIER Superinterfaces EnumBody                            { $$ = createNode("ENUM", $2, {$3, $4}); }
    | MULTI_ClassModifier ENUM IDENTIFIER EnumBody                        { $$ = createNode("ENUM", $3, {$1, $4}); }
    | ENUM IDENTIFIER EnumBody                                            { $$ = createNode("ENUM", $2, {$3}); }
    ;

EnumBody
    : '{' EnumConstantList EnumBodyDeclarations '}'                       { $$ = createNode("EnumBody", "", {$2, $3}); }
    | '{' EnumConstantList ',' EnumBodyDeclarations '}'                   { $$ = createNode("EnumBody", "", {$2, $4}); }
    | '{' EnumBodyDeclarations '}'                                        { $$ = createNode("EnumBody", "", {$2}); }
    | '{' ',' EnumBodyDeclarations '}'                                    { $$ = createNode("EnumBody", "", {$3}); }
    | '{' EnumConstantList '}'                                            { $$ = createNode("EnumBody", "", {$2}); }
    | '{' EnumConstantList ',' '}'                                        { $$ = createNode("EnumBody", "", {$2}); }
    | '{' '}'                                                             { $$ = createNode("EMP", "", {}); }
    | '{' ',' '}'                                                         { $$ = createNode("EMP", "", {}); }
    ;

EnumConstantList
    : EnumConstantList ',' EnumConstant               { $$ = $1; $$->children.push_back($3); }
    | EnumConstant                                    { $$ = createNode("EnumConstantList", "", {$1}); }
    ;

EnumConstant
    : IDENTIFIER '(' ArgumentList ')' ClassBody       { $$ = createNode("EnumConstant", $1, {$3, $5}); }
    | IDENTIFIER ClassBody                            { $$ = createNode("EnumConstant", $1, {$2}); }
    | IDENTIFIER '(' ArgumentList ')'                 { $$ = createNode("EnumConstant", $1, {$3}); }
    | IDENTIFIER                                      { $$ = createNode("EnumConstant", $1, {}); }
    ;

EnumBodyDeclarations
    : ';' MULTI_ClassBodyDeclaration                  { $$ = $2; }
    | ';'                                             { $$ = createNode("EMP", "", {}); }
    ;




/* PRODUCTION #9 (Interfaces) ----------------------------------------------- */

InterfaceDeclaration
    : NormalInterfaceDeclaration                                                   { $$ = $1; }
    ;

NormalInterfaceDeclaration
    : MULTI_ClassModifier INTERFACE IDENTIFIER ExtendsInterfaces InterfaceBody     { $$ = createNode("Interface", $3, {$1, $4, $5}); }
    | MULTI_ClassModifier INTERFACE IDENTIFIER InterfaceBody                       { $$ = createNode("Interface", $3, {$1, $4}); }
    | INTERFACE IDENTIFIER ExtendsInterfaces InterfaceBody                         { $$ = createNode("Interface", $2, {$3, $4}); }
    | INTERFACE IDENTIFIER InterfaceBody                                           { $$ = createNode("Interface", $2, {$3}); }
    ;

ExtendsInterfaces
    : EXTENDS InterfaceTypeList                                                    { $$ = $2; }
    ;

InterfaceBody
    : '{' MULTI_InterfaceMemberDeclaration '}'                                     { $$ = $2; }
    | '{' '}'                                                                      { $$ = createNode("EMP", "", {}); }
    ;

InterfaceMemberDeclaration
    : ConstantDeclaration                                     { $$ = $1; }
    | InterfaceMethodDeclaration                              { $$ = $1; }
    | ClassDeclaration                                        { $$ = $1; }
    | InterfaceDeclaration                                    { $$ = $1; }
    | ';'                                                     { $$ = createNode("EMP", "", {}); }
    ;

ConstantDeclaration
    : MULTI_ClassModifier Type VariableDeclaratorList ';'     { $$ = createNode("ConstantDeclaration", "", {$1, $2, $3}); }
    | Type VariableDeclaratorList ';'                         { $$ = createNode("ConstantDeclaration", "", {$1, $2}); }
    ;

InterfaceMethodDeclaration
    : MULTI_ClassModifier MethodHeader MethodBody             {$$ = $2; $2->children.insert($2->children.begin(), $1); $$->children.push_back($3); } 
    | MethodHeader MethodBody                                 {$$ = $1; $$->children.push_back($2); }
    ;






/* PRODUCTION #14 (Blocks and Statements) ----------------------------------------------- */

Block
    : '{' BlockStatements '}'                { $$ = $2; }
    | '{' '}'                                { $$ = createNode("EMP", "", {}); }
    ;

BlockStatements
    : BlockStatement                         { $$ = createNode("BlockStatements", "", {$1}); }
    | BlockStatements BlockStatement         { $$ = $1; $$->children.push_back($2); }
    ;

BlockStatement
    : LocalVariableDeclarationStatement      { $$ = $1; }
    | ClassDeclaration                       { $$ = $1; }
    | Statement                              { $$ = $1; }
    ;

LocalVariableDeclarationStatement
    : LocalVariableDeclaration ';'           { $$ = $1; }
    ;

LocalVariableDeclaration
    : FINAL Type VariableDeclaratorList      { Node *temp = createNode("FINAL", $1, {}); $$ = createNode("LocalVariableDeclaration", "", {temp, $2, $3}); }
    | Type VariableDeclaratorList            { $$ = createNode("LocalVariableDeclaration", "", {$1, $2}); }
    ;

Statement
    : StatementWithoutTrailingSubstatement   { $$ = $1; }
    | LabeledStatement                       { $$ = $1; }
    | IfThenStatement                        { $$ = $1; }
    | IfThenElseStatement                    { $$ = $1; }
    | WhileStatement                         { $$ = $1; }
    | ForStatement                           { $$ = $1; }
    ;

StatementNoShortIf
    : StatementWithoutTrailingSubstatement   { $$ = $1; }
    | LabeledStatementNoShortIf              { $$ = $1; }
    | IfThenElseStatementNoShortIf           { $$ = $1; }
    | WhileStatementNoShortIf                { $$ = $1; }
    | ForStatementNoShortIf                  { $$ = $1; }
    ;

StatementWithoutTrailingSubstatement
    : Block                                  { $$ = $1; }
    | EmptyStatement                         { $$ = $1; }
    | ExpressionStatement                    { $$ = $1; }
    | AssertStatement                        { $$ = $1; }
    | SwitchStatement                        { /*$$ = $1;*/ }
    | DoStatement                            { /*$$ = $1;*/ }
    | BreakStatement                         { $$ = $1; }
    | ContinueStatement                      { $$ = $1; }
    | ReturnStatement                        { $$ = $1; }
    | SynchronizedStatement                  { $$ = $1; }
    | ThrowStatement                         { $$ = $1; }
    | TryStatement                           { $$ = $1; }
    ;

EmptyStatement
    : ';'                                    { $$ = createNode("EMP", "", {}); }
    ;

LabeledStatement
    : IDENTIFIER ':' Statement               { $$ = createNode("LabeledStatement", $1, {$3}); }
    ;

LabeledStatementNoShortIf
    : IDENTIFIER ':' StatementNoShortIf      { $$ = createNode("LabeledStatement", $1, {$3}); }
    ;

ExpressionStatement
    : StatementExpression ';'                { $$ = $1; }
    ;

StatementExpression
    : Assignment                             { $$ = $1; }
    | PreIncrementExpression                 { $$ = $1; }
    | PreDecrementExpression                 { $$ = $1; }
    | PostIncrementExpression                { $$ = $1; }
    | PostDecrementExpression                { $$ = $1; }
    | MethodInvocation                       { $$ = $1; }
    | ClassInstanceCreationExpression        { $$ = $1; }
    ;

IfThenStatement
    : IF '(' Expression ')' Statement                                       {$5->e_label="true";$3->e_label="condition";$$=createNode("IFTHEN",$1,{$3,$5});}
    ;

IfThenElseStatement
    : IF '(' Expression ')' StatementNoShortIf ELSE Statement               {$5->e_label="true";$7->e_label="false";$3->e_label="condition";$$=createNode("IFTHENELSE",$1,{$3,$5,$7});}
    ;

IfThenElseStatementNoShortIf
    : IF '(' Expression ')' StatementNoShortIf ELSE StatementNoShortIf      {$5->e_label="true";$7->e_label="false";$3->e_label="condition";$$=createNode("IFTHENELSE",$1,{$3,$5,$7});}
    ;

AssertStatement
    : ASSERT Expression ';'                                                 {$$=createNode("ASSERT",$1,{$2});}
    | ASSERT Expression ':' Expression ';'                                  {$4->e_label="MESSAGE";$$=createNode("ASSERT",$1,{$2,$4});}
    ;

SwitchStatement
    : SWITCH '(' Expression ')' SwitchBlock                                 {/*$$=createNode("SWITCH",$1,{$3,$5});*/}
    ;

SwitchBlock
    : '{' MULTI_SwitchBlockStatementGroup MULTI_SwitchLabel '}'             {/*$$=createNode("SwitchBlock","",{$2,$3});*/}
    | '{' MULTI_SwitchLabel '}'                                             {/*$$=createNode("SwitchBlock","",{$2});*/}
    | '{' MULTI_SwitchBlockStatementGroup '}'                               {/*$$=createNode("SwitchBlock","",{$2});*/}
    | '{' '}'                                                               {/*$$=createNode("EMP","",{});*/}
    ;

SwitchBlockStatementGroup
    : MULTI_SwitchLabel BlockStatements                                     {/*$$=createNode("SwitchBlockStatementGroup","",{$1,$2});*/}
    ;

SwitchLabel
    : CASE Expression ':'                                                   {/*$$=createNode("SwitchLabel",$1,{$2});*/} //////////////////////////////////////////////////
    // | CASE IDENTIFIER ':'
    | DEFAULT ':'                                                           {/*$$=createNode("SwitchLabel",$1,{});*/}////////////////////////////////////////////////////
    ;

WhileStatement
    : WHILE '(' Expression ')' Statement                                    {$3->e_label="condition";$5->e_label="true";$$=createNode("WHILE",$1,{$3,$5});}
    ;

WhileStatementNoShortIf
    : WHILE '(' Expression ')' StatementNoShortIf                           {$3->e_label="condition";$5->e_label="true";$$=createNode("WHILE",$1,{$3,$5});}
    ;

DoStatement
    : DO Statement WHILE '(' Expression ')' ';'                             {/*$5->e_label="condition";$2->e_label="true";$$=createNode("DoWhile",$)*/}
    ;

ForStatement                                                                
    : BasicForStatement                          {$$=$1;}
    | EnhancedForStatement                       {$$=$1;}
    ;

ForStatementNoShortIf
    : BasicForStatementNoShortIf                 {$$=$1;}
    | EnhancedForStatementNoShortIf              {$$=$1;}
    ;

BasicForStatement
    : FOR '(' SINGLE_ForInit ';' SINGLE_Expression ';' SINGLE_ForUpdate ')' Statement    {$3->e_label="initializer";$5->e_label="condition";
                                                                                          $7->e_label="updater";$9->e_label="body";
                                                                                          $$=createNode("FOR",$1,{$3,$5,$7,$9});}
    ;

SINGLE_ForInit
    : /* Empty */          {$$=createNode("EMP","",{});}
    | ForInit              {$$=$1;}
    ;

SINGLE_Expression
    : /* Empty */          {$$=createNode("EMP","",{});}
    | Expression           {$$=$1;}
    ;

SINGLE_ForUpdate
    : /* Empty */          {$$=createNode("EMP","",{});}
    | ForUpdate            {$$=$1;}
    ;

BasicForStatementNoShortIf
    : FOR '(' SINGLE_ForInit ';' SINGLE_Expression ';' SINGLE_ForUpdate ')' StatementNoShortIf   {$3->e_label="initializer";$5->e_label="condition";
                                                                                                  $7->e_label="updater";$9->e_label="body";
                                                                                                  $$=createNode("FOR",$1,{$3,$5,$7,$9});}
    ;

ForInit
    : StatementExpressionList          {$$=$1;}
    | LocalVariableDeclaration         {$$=$1;}
    ;

ForUpdate
    : StatementExpressionList          {$$=$1;}
    ;

StatementExpressionList
    : StatementExpression                               {$$=createNode("StatementExpressions", "", {$1});}
    | StatementExpressionList ',' StatementExpression   {$1->children.push_back($3);$$=$1;}
    ;

EnhancedForStatement
    : FOR '(' FINAL Type VariableDeclaratorId ':' Expression ')' Statement    {Node *temp1 = createNode("KEYWORD",$3,{});
                                                                               Node *temp = createNode("PARAMETERS","",{temp1,$4,$5});
                                                                               Node *temp2 = createNode("OP",":",{temp,$7});
                                                                               $$=createNode("FOR",$1,{temp2,$9});
                                                                               $9->e_label="body";}
    | FOR '(' Type VariableDeclaratorId ':' Expression ')' Statement          {Node *temp = createNode("PARAMETERS","",{$3,$4});
                                                                               Node *temp1 = createNode("OP",":",{temp,$6});
                                                                               $$=createNode("FOR",$1,{temp1,$8});
                                                                               $8->e_label="body";}
    ;

EnhancedForStatementNoShortIf
    : FOR '(' FINAL Type VariableDeclaratorId ':' Expression ')' StatementNoShortIf {Node *temp1 = createNode("KEYWORD",$3,{});
                                                                                     Node *temp = createNode("PARAMETERS","",{temp1,$4,$5});
                                                                                     Node *temp2 = createNode("OP",":",{temp,$7});
                                                                                     $$=createNode("FOR",$1,{temp2,$9});
                                                                                     $9->e_label="body";}
    | FOR '(' Type VariableDeclaratorId ':' Expression ')' StatementNoShortIf       {Node *temp = createNode("PARAMETERS","",{$3,$4});
                                                                                     Node *temp1 = createNode("OP",":",{temp,$6});
                                                                                     $$=createNode("FOR",$1,{temp1,$8});
                                                                                     $8->e_label="body";}
    ;

BreakStatement
    : BREAK IDENTIFIER ';'        {Node* temp=createNode("ID",$2,{});$$=createNode("BREAK",$1,{temp});}
    | BREAK ';'                   {$$=createNode("BREAK",$1,{});}
    ;

ContinueStatement
    : CONTINUE IDENTIFIER ';'     {Node* temp=createNode("ID",$2,{});$$=createNode("CONTINUE",$1,{temp});}
    | CONTINUE ';'                {$$=createNode("CONTINUE",$1,{});}
    ;

ReturnStatement
    : RETURN Expression ';'       {$$=createNode("RETURN",$1,{$2});}
    | RETURN ';'                  {$$=createNode("RETURN",$1,{});}
    ;

ThrowStatement
    : THROW Expression ';'        {$$=createNode("THROW",$1,{$2});}
    ;

SynchronizedStatement
    : SYNCHRONIZED '(' Expression ')' Block  {$$=createNode("SYNCHRONIZED",$1,{$3,$5});$3->e_label="expression";$5->e_label="body";}
    ;

TryStatement
    : TRY Block Catches                                 {$$ = createNode("TRYSTATEMENT","",{$2,$3});$2->e_label="try";}
    | TRY Block Catches Finally                         {$$ = createNode("TRYSTATEMENT","",{$2,$3,$4});$2->e_label="try";}
    | TRY Block Finally                                 {$$ = createNode("TRYSTATEMENT","",{$2,$3});$2->e_label="try";}
    | TryWithResourcesStatement                         {$$ = $1;}
    ;

Catches
    : CatchClause                                       {$$ = createNode("CATCHES","", {$1});}
    | Catches CatchClause                               {$1->children.push_back($2); $$ = $1;}
    ;

CatchClause
    : CATCH '(' CatchFormalParameter ')' Block          {$$ = createNode("CATCH",$1,{$3,$5});
                                                         $3->e_label="exception";$5->e_label="body";}
    ;

CatchFormalParameter
    : FINAL CatchType VariableDeclaratorId              {Node *temp = createNode("KEYWORD",$1,{});
                                                         $$ = createNode("CATCH_PARAMETER","",{temp,$2,$3});}
    | CatchType VariableDeclaratorId                    {$$ = createNode("CATCH_PARAMETER","",{$1,$2});}
    ;

CatchType
    : /* Treat as Unann Class Type */TypeName           {$$ = createNode("CATCH_TYPE","", {$1});}           
    | CatchType '|' /* Treat as Class Type */TypeName   {$1->children.push_back($3); $$ = $1;}
    ;

Finally
    : FINALLY Block                                     {$$ = createNode("FINALLY","",{$2});}
    ;

TryWithResourcesStatement
    : TRY ResourceSpecification Block Catches Finally  {$$ = createNode("Try_with_resources",$1,{$2,$3,$4,$5});}
    | TRY ResourceSpecification Block Catches          {$$ = createNode("Try_with_resources",$1,{$2,$3,$4});}
    | TRY ResourceSpecification Block Finally          {$$ = createNode("Try_with_resources",$1,{$2,$3,$4});}
    | TRY ResourceSpecification Block                  {$$ = createNode("Try_with_resources",$1,{$2,$3});}
    ;

ResourceSpecification
    : '(' ResourceList ';' ')'                         {$$ = $2;}
    | '(' ResourceList ')'                             {$$ = $2;}
    ;

ResourceList
    : Resource                                         {$$ = createNode("Resources","",{$1});}
    | ResourceList ';' Resource                        {$$ = $1; $$->children.push_back($3);}
    ;

Resource
    : FINAL Type VariableDeclaratorId '=' Expression   {Node *temp = createNode("Resource",$1,{$2,$3});
                                                        $$ = createNode("OP",$4,{temp,$5});}
    | Type VariableDeclaratorId '=' Expression         {Node *temp = createNode("Resource","",{$1,$2});
                                                        $$ = createNode("OP",$3,{temp,$4});}
    ;









/* PRODUCTION #15 (Expressions) ----------------------------------------------- */

Primary
    : PrimaryNoNewArray                                                             {$$=$1;}
    | ArrayCreationExpression                                                       {$$=$1;}
    ;

PrimaryNoNewArray
    : LITERAL                                                                       {$$=createNode("LITERAL",$1,{});}
    | ClassLiteral                                                                  {$$=$1;}
    | THIS                                                                          {$$=createNode("KEYWORD",$1,{});}
    | TypeName '.' THIS                                                             {Node *temp = createNode("KEYWORD",$3,{});
                                                                                     $$=createNode("PRIMARYNONEWARRAY","",{$1,temp});}                                                             
    | '(' Expression ')'                                                            {$$=$2;}
    | ClassInstanceCreationExpression                                               {$$=$1;}
    | FieldAccess                                                                   {$$=$1;}
    | ArrayAccess                                                                   {$$=$1;}
    | MethodInvocation                                                              {$$=$1;}
    | MethodReference                                                               {$$=$1;}
    ;

ClassLiteral
    : TypeName Dims '.' CLASS                                                       {Node *temp = createNode("KEYWORD",$4,{});
                                                                                     $$ = createNode("CLASS_LITERAL","",{$1,$2,temp});}
    | TypeName '.' CLASS                                                            {Node *temp = createNode("KEYWORD",$3,{});
                                                                                     $$ = createNode("CLASS_LITERAL","",{$1,temp});}
    | PrimitiveType Dims '.' CLASS                                                  {Node *temp = createNode("KEYWORD",$4,{});
                                                                                     $$ = createNode("CLASS_LITERAL","",{$1,$2,temp});}
    | PrimitiveType '.' CLASS /* combined numerictype and boolean */                {Node *temp = createNode("KEYWORD",$3,{});
                                                                                     $$ = createNode("CLASS_LITERAL","",{$1,temp});}
    | VOID '.' CLASS                                                                {Node *temp = createNode("KEYWORD",$3,{});
                                                                                     Node *temp1 = createNode("KEYWORD",$1,{});
                                                                                     $$ = createNode("CLASS_LITERAL","",{temp1,temp});}
    ;

ClassInstanceCreationExpression
    : UnqualifiedClassInstanceCreationExpression                                         {$$=$1;}
    | /*treat as expression name*/TypeName '.' UnqualifiedClassInstanceCreationExpression{$$=createNode("CLASS_INSTANCE","",{$1,$3});$1->label="Expression Name";}
    | Primary '.' UnqualifiedClassInstanceCreationExpression                             {$$=createNode("CLASS_INSTANCE","",{$1,$3});}
    ;

UnqualifiedClassInstanceCreationExpression
    : NEW TypeName '(' ArgumentList ')'                                                 {Node *temp = createNode("KEYWORD",$1,{});
                                                                                        $$ = createNode("UNQUA_CLASS_INSTANCE","",{temp,$2,$4});$2->label="ClassType";}
    | NEW TypeName '(' ArgumentList ')' ClassBody                                       {Node *temp = createNode("KEYWORD",$1,{});
                                                                                        $$ = createNode("UNQUA_CLASS_INSTANCE","",{temp,$2,$4,$6});$2->label="ClassType";}
    | NEW TypeName '(' ')' ClassBody                                                    {Node *temp = createNode("KEYWORD",$1,{});
                                                                                        Node *temp1 = createNode("EMP","",{});
                                                                                        $$ = createNode("UNQUA_CLASS_INSTANCE","",{temp,$2,temp1,$5});$2->label="ClassType";}
    | NEW TypeName '(' ')'                                                              {Node *temp = createNode("KEYWORD",$1,{});
                                                                                        Node *temp1 = createNode("EMP","",{});
                                                                                        $$ = createNode("UNQUA_CLASS_INSTANCE","",{temp,$2,temp1});$2->label="ClassType";}
    ;/*in each treat typename as classorinterfacetypetoinstantiate */

FieldAccess
    : Primary '.' IDENTIFIER                                                           {Node *temp = createNode("ID",$3,{});
                                                                                        $$ = createNode("FIELDACC","",{$1,temp});}
    | SUPER '.' IDENTIFIER                                                              {Node *temp = createNode("KEYWORD",$1,{});
                                                                                        Node *temp1 = createNode("ID",$3,{});
                                                                                        $$ = createNode("FIELDACC","",{temp,temp1});}
    | TypeName '.' SUPER '.' IDENTIFIER                                                 {Node *temp = createNode("KEYWORD",$3,{});
                                                                                        Node *temp1 = createNode("ID",$5,{});
                                                                                        $$ = createNode("FIELDACC","",{$1,temp,temp1});}
    ;

ArrayAccess
    : /*treat as expression name*/TypeName '[' Expression ']'                          {$$=createNode("ARRAY","",{$1,$3});$1->label="Expression Name";}
    | PrimaryNoNewArray '[' Expression ']'                                             {$$=createNode("ARRAY","",{$1,$3});}
    ;

MethodInvocation
    : IDENTIFIER '(' ArgumentList ')'                                                  {Node *temp = createNode("ID",$1,{});
                                                                                        $$=createNode("METHODINVOCATION","",{temp,$3});}
    | IDENTIFIER '(' ')'                                                               {Node *temp = createNode("ID",$1,{});
                                                                                        Node *temp1 = createNode("EMP","",{});
                                                                                        $$=createNode("METHODINVOCATION","",{temp,temp1});}
    | /*treat both as typename and expression name*/TypeName '.' IDENTIFIER '(' ArgumentList ')'
                                                                                       {Node *temp = createNode("ID",$3,{});
                                                                                        $$=createNode("METHODINVOCATION","",{$1,temp,$5});}
    | /*treat both as typename and expression name*/TypeName '.' IDENTIFIER '(' ')'
                                                                                        {Node *temp = createNode("ID",$3,{});
                                                                                        Node *temp1 = createNode("EMP","",{});
                                                                                        $$=createNode("METHODINVOCATION","",{$1,temp,temp1});}
    | Primary '.' IDENTIFIER '(' ArgumentList ')'                                       {Node *temp = createNode("ID",$3,{});
                                                                                        $$=createNode("METHODINVOCATION","",{$1,temp,$5});}
    | Primary '.' IDENTIFIER '(' ')'                                                    {Node *temp = createNode("ID",$3,{});
                                                                                        Node *temp1 = createNode("EMP","",{});
                                                                                        $$=createNode("METHODINVOCATION","",{$1,temp,temp1});}
    | SUPER '.' IDENTIFIER '(' ArgumentList ')'                                         {Node *temp = createNode("ID",$3,{});
                                                                                        Node *temp1 = createNode("KEYWORD",$1,{});
                                                                                        $$=createNode("METHODINVOCATION","",{temp1,temp,$5});}
    | SUPER '.' IDENTIFIER '(' ')'                                                      {Node *temp = createNode("ID",$3,{});
                                                                                        Node *temp1 = createNode("KEYWORD",$1,{});
                                                                                        Node *temp2 = createNode("EMP","",{});
                                                                                        $$=createNode("METHODINVOCATION","",{temp1,temp,temp2});}
    | TypeName '.' SUPER '.' IDENTIFIER '(' ArgumentList ')'                            {Node *temp = createNode("ID",$5,{});
                                                                                        Node *temp1 = createNode("KEYWORD",$3,{});
                                                                                        $$=createNode("METHODINVOCATION","",{$1,temp1,temp,$7});}
    | TypeName '.' SUPER '.' IDENTIFIER '(' ')'                                         {Node *temp = createNode("ID",$5,{});
                                                                                        Node *temp1 = createNode("KEYWORD",$3,{});
                                                                                        Node *temp2 = createNode("EMP","",{});
                                                                                        $$=createNode("METHODINVOCATION","",{$1,temp1,temp,temp2});}
    ;

ArgumentList
    : ArgumentList ',' Expression                                                      {$1->children.push_back($3); $$ = $1; }
    | Expression                                                                       {$$ = createNode("ARGLIST","", {$1}); }
    ;

MethodReference
   : /*treat as expression name and class type*/TypeName PROPORTION IDENTIFIER          {Node *temp = createNode("ID",$3,{});
                                                                                         $$ = createNode("METHOD_REF","",{$1,temp});}
   | ArrayType PROPORTION IDENTIFIER                                                    {Node *temp = createNode("ID",$3,{});
                                                                                         $$ = createNode("METHOD_REF","",{$1,temp});}
   | Primary PROPORTION IDENTIFIER                                                      {Node *temp = createNode("ID",$3,{});
                                                                                         $$ = createNode("METHOD_REF","",{$1,temp});}
   | SUPER PROPORTION IDENTIFIER                                                        {Node *temp = createNode("ID",$3,{});
                                                                                         Node *temp1 = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("METHOD_REF","",{temp1,temp});}
   | TypeName '.' SUPER PROPORTION IDENTIFIER                                           {Node *temp = createNode("ID",$5,{});
                                                                                         Node *temp1 = createNode("KEYWORD",$3,{});
                                                                                         $$ = createNode("METHOD_REF","",{$1,temp1,temp});}
   | /*treat as class type*/TypeName PROPORTION NEW                                     {Node *temp = createNode("KEYWORD",$3,{});
                                                                                         $$ = createNode("METHOD_REF","",{$1,temp});$1->label="ClassType";}
   | ArrayType PROPORTION NEW                                                           {Node *temp = createNode("KEYWORD",$3,{});
                                                                                         $$ = createNode("METHOD_REF","",{$1,temp});}
   ;

ArrayCreationExpression
   : NEW PrimitiveType DimExprs Dims                                                    {Node *temp = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("ARRAY_CREATION","",{temp,$2,$3,$4});}
   | NEW PrimitiveType DimExprs                                                         {Node *temp = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("ARRAY_CREATION","",{temp,$2,$3});}                                      
   | NEW /*treat as class type*/TypeName DimExprs Dims                                  {Node *temp = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("ARRAY_CREATION","",{temp,$2,$3,$4});$2->label="ClassType";}
   | NEW /*treat as class type*/TypeName DimExprs                                       {Node *temp = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("ARRAY_CREATION","",{temp,$2,$3});$2->label="ClassType";}
   | NEW PrimitiveType Dims ArrayInitializer                                            {Node *temp = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("ARRAY_CREATION","",{temp,$2,$3,$4});}
   | NEW /*treat as class type*/TypeName Dims ArrayInitializer                          {Node *temp = createNode("KEYWORD",$1,{});
                                                                                         $$ = createNode("ARRAY_CREATION","",{temp,$2,$3,$4});$2->label="ClassType";}
   ;

DimExprs
   : DimExpr                                                                            {$$ = createNode("DIMEXPR","", {$1}); }
   | DimExprs DimExpr                                                                   {$1->children.push_back($2); $$ = $1; }
   ;

DimExpr
   : '[' Expression ']'                                                                 {$$=$2;}
   ;

Expression
   : LambdaExpression                                                                   {$$=$1;}
   | AssignmentExpression                                                               {$$=$1;}
   ;

LambdaExpression
   : IDENTIFIER PTR_OP LambdaBody                                                       {Node *temp = createNode("ID",$1,{});
                                                                                         $$ = createNode("OP",$2,{temp,$3});}
   | LambdaParameters PTR_OP LambdaBody                                                 {$$ = createNode("OP",$2,{$1,$3});}
   ;

LambdaParameters
   : '(' ')'                                                                            {$$ = createNode("EMP","",{});}
   | '(' FormalParameterList ')'                                                        {$$ = $2;}/* assuming node for formal.. */
   // | '(' InferredFormalParameterList ')'
   ;

// InferredFormalParameterList
//    : IDENTIFIER
//    | InferredFormalParameterList ',' IDENTIFIER
//    ;

LambdaBody
   : Expression                                                                         {$$=$1;}
   | Block                                                                              {$$=$1;}
   ;

AssignmentExpression
   : ConditionalExpression                                                              {$$=$1;}                    
   | Assignment                                                                         {$$=$1;}
   ;

Assignment
   : LeftHandSide AssignmentOperator Expression                                         {$2->children={$1,$3};$$=$2;}
   | /*treat as expression name*/TypeName AssignmentOperator Expression                 {$2->children={$1,$3};$$=$2;$1->label="Expression Name";}
   ;

LeftHandSide
   : FieldAccess                                                                        {$$=$1;}
   | ArrayAccess                                                                        {$$=$1;}
   ;

AssignmentOperator
   : '='                                                                                {$$=createNode("OP",$1,{});}
   | MUL_ASSIGN                                                                         {$$=createNode("OP",$1,{});}
   | DIV_ASSIGN                                                                         {$$=createNode("OP",$1,{});}
   | MOD_ASSIGN                                                                         {$$=createNode("OP",$1,{});}
   | ADD_ASSIGN                                                                         {$$=createNode("OP",$1,{});}                                                                               
   | SUB_ASSIGN                                                                         {$$=createNode("OP",$1,{});}
   | LEFT_ASSIGN                                                                        {$$=createNode("OP",$1,{});}
   | RIGHT_ASSIGN                                                                       {$$=createNode("OP",$1,{});}
   | UNSIGNED_RIGHT_ASSIGN                                                              {$$=createNode("OP",$1,{});}
   | AND_ASSIGN                                                                         {$$=createNode("OP",$1,{});}  
   | XOR_ASSIGN                                                                         {$$=createNode("OP",$1,{});}
   | OR_ASSIGN                                                                          {$$=createNode("OP",$1,{});}
   ;

ConditionalExpression
   : ConditionalOrExpression                                                            {$$=$1;}
   | ConditionalOrExpression '?' Expression ':' ConditionalExpression                   {$$=createNode("BRANCH","",{$1,$3,$5});
                                                                                         $1->e_label="condition";
                                                                                         $3->e_label="then";
                                                                                         $5->e_label="else";}
   | ConditionalOrExpression '?' Expression ':' LambdaExpression                        {$$=createNode("BRANCH","",{$1,$3,$5});
                                                                                         $1->e_label="condition";
                                                                                         $3->e_label="then";
                                                                                         $5->e_label="else";}                        
   ;

ConditionalOrExpression
    : ConditionalAndExpression                                                          {$$=$1;}
   | ConditionalOrExpression OR_OP ConditionalAndExpression                             {$$=createNode("OP",$2,{$1,$3});}
   ;

ConditionalAndExpression
   : InclusiveOrExpression                                                              {$$=$1;}                                                                                                                                                                                                                           
   | ConditionalAndExpression AND_OP InclusiveOrExpression                              {$$=createNode("OP",$2,{$1,$3});}                                                                                                                                                               
   ;

InclusiveOrExpression
   : ExclusiveOrExpression                                                              {$$=$1;}                                        
   | InclusiveOrExpression '|' ExclusiveOrExpression                                    {$$=createNode("OP",$2,{$1,$3});}                                                                        
   ;

ExclusiveOrExpression
   : AndExpression                                                                      {$$=$1;}                              
   | ExclusiveOrExpression '^' AndExpression                                            {$$=createNode("OP",$2,{$1,$3});}                                                               
   ;

AndExpression
   : EqualityExpression                                                                 {$$=$1;}                                  
   | AndExpression '&' EqualityExpression                                               {$$=createNode("OP",$2,{$1,$3});}                                                           
   ;

EqualityExpression
   : RelationalExpression                                                               {$$=$1;}
   | EqualityExpression EQ_OP RelationalExpression                                      {$$=createNode("OP",$2,{$1,$3});}
   | EqualityExpression NE_OP RelationalExpression                                      {$$=createNode("OP",$2,{$1,$3});}
   ;

RelationalExpression
   : ShiftExpression                                                                    {$$=$1;}
   | RelationalExpression '<' ShiftExpression                                           {$$=createNode("OP",$2,{$1,$3});}
   | RelationalExpression '>' ShiftExpression                                           {$$=createNode("OP",$2,{$1,$3});}
   | RelationalExpression LE_OP ShiftExpression                                         {$$=createNode("OP",$2,{$1,$3});}
   | RelationalExpression GE_OP ShiftExpression                                         {$$=createNode("OP",$2,{$1,$3});}
   | RelationalExpression INSTANCEOF ReferenceType                                      {$$=createNode("OP",$2,{$1,$3});}
   ;

ShiftExpression
   : AdditiveExpression                                                                 {$$=$1;}
   | ShiftExpression LEFT_OP  AdditiveExpression                                        {$$=createNode("OP",$2,{$1,$3});}
   | ShiftExpression RIGHT_OP AdditiveExpression                                        {$$=createNode("OP",$2,{$1,$3});}
   | ShiftExpression UNSIGNED_RIGHT_OP AdditiveExpression                               {$$=createNode("OP",$2,{$1,$3});}
   ;

AdditiveExpression
   : MultiplicativeExpression                                                           {$$=$1;}
   | AdditiveExpression '+' MultiplicativeExpression                                    {$$=createNode("OP",$2,{$1,$3});}
   | AdditiveExpression '-' MultiplicativeExpression                                    {$$=createNode("OP",$2,{$1,$3});}
   ;

MultiplicativeExpression
   : UnaryExpression                                                                    {$$=$1;}
   | MultiplicativeExpression '*' UnaryExpression                                       {$$=createNode("OP",$2,{$1,$3});}                 
   | MultiplicativeExpression '/' UnaryExpression                                       {$$=createNode("OP",$2,{$1,$3});}
   | MultiplicativeExpression '%' UnaryExpression                                       {$$=createNode("OP",$2,{$1,$3});}
   ;

UnaryExpression
   : PreIncrementExpression                                                             {$$=$1;}
   | PreDecrementExpression                                                             {$$=$1;}
   | '+' UnaryExpression                                                                {Node * temp = createNode("OP",$1,{});
                                                                                         $$=createNode("UOP","",{temp,$2});}
   | '-' UnaryExpression                                                                {Node * temp = createNode("OP",$1,{});
                                                                                         $$=createNode("UOP","",{temp,$2});}
   | UnaryExpressionNotPlusMinus                                                        {$$=$1;}
   ;

PreIncrementExpression
   : INC_OP UnaryExpression                                                             {Node * temp = createNode("OP",$1,{});
                                                                                         $$=createNode("UOP","",{temp,$2});}
   ;

PreDecrementExpression
   : DEC_OP UnaryExpression                                                             {Node * temp = createNode("OP",$1,{});
                                                                                         $$=createNode("UOP","",{temp,$2});}
   ;

UnaryExpressionNotPlusMinus
   : PostfixExpression                                                                  {$$=$1;}
   | '~' UnaryExpression                                                                {Node * temp = createNode("OP",$1,{});
                                                                                         $$=createNode("UOP","",{temp,$2});}
   | '!' UnaryExpression                                                                {Node * temp = createNode("OP",$1,{});
                                                                                         $$=createNode("UOP","",{temp,$2});}
   | CastExpression                                                                     {$$=$1;}
   ;

PostfixExpression
   : Primary                                                                            {$$=$1;}
   | /*treat as expression name*/TypeName                                               {$$=$1;$1->label="Expression Name";}
   | PostIncrementExpression                                                            {$$=$1;}
   | PostDecrementExpression                                                            {$$=$1;}
   ;

PostIncrementExpression
   : PostfixExpression INC_OP                                                           {Node * temp = createNode("OP",$2,{});
                                                                                         $$=createNode("UOP","",{$1,temp});}
   ;

PostDecrementExpression
   : PostfixExpression DEC_OP                                                           {Node * temp = createNode("OP",$2,{});
                                                                                         $$=createNode("UOP","",{$1,temp});}
   ;

CastExpression
   : '(' PrimitiveType ')' UnaryExpression                                              {$$=createNode("TYPECAST","",{$2,$4});}
   // | '(' ReferenceType MULTI_AdditionalBound ')' UnaryExpressionNotPlusMinus
   // | '(' ReferenceType MULTI_AdditionalBound ')' LambdaExpression
   // | '(' ReferenceType ')' UnaryExpressionNotPlusMinus
   // | '(' ReferenceType ')' LambdaExpression
   ;
   /* to be handled later */











/* ------------------ OUR EXTRA STUFF -------------------------- */


MULTI_ImportDeclaration
    : ImportDeclaration                                { $$ = createNode("Imports", "", {$1}); }
    | MULTI_ImportDeclaration ImportDeclaration        { $$ = $1; $$->children.push_back($2);  }
    ;

MULTI_TypeDeclaration
    : TypeDeclaration                                  { $$ = createNode("Declarations", "", {$1}); }
    | MULTI_TypeDeclaration TypeDeclaration            { $$ = $1; $$->children.push_back($2);  }
    ;

MULTI_ClassModifier
    : ClassModifier                                    { $$ = createNode("Modifiers", "", {$1}); }
    | MULTI_ClassModifier ClassModifier                { $$ = $1; $$->children.push_back($2);  }
    ;

MULTI_ClassBodyDeclaration
    : ClassBodyDeclaration                             { $$ = createNode("ClassBody", "", {$1}); }
    | MULTI_ClassBodyDeclaration ClassBodyDeclaration  { $$ = $1; $$->children.push_back($2);  }
    ;

SINGLE_ArgumentList
    : '(' ')' ';'                                      { $$ = createNode("EMP", "", {}); }
    | '(' ArgumentList ')' ';'                         { $$ = $2; }
    ;

MULTI_InterfaceMemberDeclaration
    : InterfaceMemberDeclaration                                      { $$ = createNode("InterfaceBody", "", {$1}); }
    | MULTI_InterfaceMemberDeclaration InterfaceMemberDeclaration     { $$ = $1; $$->children.push_back($2);  }
    ;

MULTI_SwitchBlockStatementGroup
    : SwitchBlockStatementGroup                                       { $$ = createNode("SwitchBlocks", "", {$1}); }
    | MULTI_SwitchBlockStatementGroup SwitchBlockStatementGroup       { $$ = $1; $$->children.push_back($2); }
    ;

MULTI_SwitchLabel
    : SwitchLabel                                                     { $$ = createNode("SwitchLabels", "", {$1}); }
    | MULTI_SwitchLabel SwitchLabel                                   { $$ = $1; $$->children.push_back($2); }
    ;

%%                    


int counter=2;
void dfs(Node* head, int head_num){
    for(Node* u:head->children){
        string temp = (u->value)[0]=='"' ? "\\"+(u->value).substr(0,(u->value).size()-1)+"\\\"" : u->value;
        string val = (u->value).empty() ? u->label : u->label+"("+temp+")";
        string shape = (u->value).empty() ? ", shape = box" : "";
        string color = (u->value).empty() ? "" : ", color=lightblue, style=filled";
        cout << counter << " [label = \""+val+"\""+shape+color+"];\n";
        cout << head_num << "->" << counter << " [ label = \"" + u->e_label + "\"];\n";
        counter++;
        dfs(u,counter-1);
    }
}

void generate_dot(){
    if(root==NULL)  return;
    cout << "digraph G {" << '\n' << "size=\"7,15\"; center = true; ";
    string temp = (root->value)[0]=='"' ? "\\"+(root->value).substr(0,(root->value).size()-1)+"\\\"" : root->value;
    string val = (root->value).empty() ? root->label : root->label+"("+temp+")";
    string shape = (root->value).empty() ? ", shape = box" : "";
    string color = (root->value).empty() ? "" : ", color=lightblue, style=filled";
    cout << "1 [label = \""+val+"\""+shape+color+"];\n";
    dfs(root,1);
    cout << "}";
}


int main (void) {
    int ret_value = yyparse();

    if(ret_value != 0) return ret_value;
    generate_dot();

    return ret_value;
}

void yyerror (const char *s) {cerr<<"Line "<<line<<": "<<s;}
