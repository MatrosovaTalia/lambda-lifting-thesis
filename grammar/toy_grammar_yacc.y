/* BNF Declarations */

// Identifiers & numbers
%token  IDENTIFIER
%token  INTEGER_LITERAL
%token  REAL_LITERAL
%token  TRUE FALSE

// Keywords
%token END DEF
%token INTEGER REAL BOOLEAN
%token WHILE FOR IF ELSE AND
%token OR NOT XOR PRINT RETURN

// Separators
%token NEWLINE
%token SEMICOLON

// Delimiters
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT
%token COLON
%token IN_RANGE

// Operator signs
%token EQUALS
%token ASSIGN
%token NEQUALS
%token GREATER
%token LESS
%token LEQUALS
%token GEQUALS
%token ADD
%token SUB
%token MUL
%token DIV
%token REM

%type  Program
%type  RoutineDeclaration
%type  Return
%type  Print
%type  Parameters
%type  Array
%type  Body
%type  BodyDeclaration
%type  Statement
%type  Assignment
%type  RoutineCall
%type  Expressions
%type  Expression
%type  WhileLoop
%type  ForLoop
%type  IfStatement
%type  Relations
%type  LogicWord
%type  Relation
%type  LogicTail
%type  RelationSign
%type  Logic
%type  FactorSign
%type  FactorTail
%type  Factor
%type  SummandSign
%type  Summand
%type  SummandTail
%type  Primary
%type  Identifier

%start Program

%%

Program
    : /* empty */ 
    | Statement Program
    ;

RoutineDeclaration
    : DEF Identifier LPAREN Parameters RPAREN Body END  
    | DEF Identifier LPAREN Parameters RPAREN COLON Body END 
    ;

Parameters
    : /* empty */ 
    | Identifier COMMA Parameters 
    ;

Array
    : LBRACKET Primaries RBRACKET  
    ;

ArrayElement
    : Identifier LBRACKET Expression RBRACKET
    ;

Primaries
    : Primary
    | COMMA Primaries
    ;

Body
    : /* empty */ 
    | BodyDeclaration Body
    ;

BodyDeclaration
    : Return 
    | Statement
    ;

Statement
    : Assignment  
    | RoutineCall  
    | WhileLoop  
    | ForLoop  
    | IfStatement 
    | Print  
    | RoutineDeclaration  
    ;

Return
    : RETURN Expression
    ;

Assignment
    : Identifier EQUALS Expression 
    ;

RoutineCall
    : Identifier LPAREN Expressions RPAREN
    ;

Expressions
    : Expression 
    | Expression COMMA Expressions 
    ;

WhileLoop
    : WHILE Expression COLON Body END 
    ;

ForLoop
    : FOR Identifier IN_RANGE LPAREN Expression COMMA Expression RPAREN COLON NEWLINE Body END
    ;

IfStatement
    : IF Expression COLON NEWLINE Body END
    | IF Expression COLON NEWLINE Body ELSE Body END
    ;


Expression
    : Relation Relations
    | SummandSign Relation Relations 
    | NOT Relation Relations 
    | NOT SummandSign Relation Relations 
    ;

Relations
    : /* empty */ 
    | LogicWord Relation Relations
    | LogicWord NOT Relation Relations 
    ;

LogicWord
    : AND
    | OR 
    | XOR 
    ;

Relation
    : Logic LogicTail 
    ;


LogicTail
    : /* empty */ 
    | RelationSign Logic LogicTail 
    ;


RelationSign
    : LESS 
    | GREATER 
    | LEQUALS 
    | GEQUALS 
    | EQUALS 
    | NEQUALS
    ;

Logic
    : Factor FactorTail 
    ;

FactorTail
    : /* empty */ 
    | FactorSign Factor FactorTail 
    ;

FactorSign
    : MUL
    | DIV
    | REM 
    ;

Factor
    : Summand SummandTail 
    ;

SummandTail
    : /* empty */ 
    | SummandSign Summand SummandTail 

SummandSign
    : ADD
    | SUB
    ;

Summand
    : Primary
    | LPAREN Expression RPAREN 
    ;

Primary
    : INTEGER_LITERAL 
    | REAL_LITERAL 
    | TRUE 
    | FALSE 
    | RoutineCall 
    | ArrayElement
    | Array
    ;

Print
    : PRINT LPAREN Expressions RPAREN
    ;

Identifier:
	IDENTIFIER