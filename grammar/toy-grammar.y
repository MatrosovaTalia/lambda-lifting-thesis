{
    module Main where
}

%name toy-grammar
%tokentype { Token }
%error {parseError}

%token
    

%%

/* BNF Declarations */


%token  
    // Identifiers & numbers
    IDENTIFIER         { TokenId }
    INTEGER_LITERAL    { TokenIntLit }
    REAL_LITERAL       { TokenRealLit }
    TRUE               { TokenTrue }
    FALSE              { TokenFalse }

    // Keywords
    END                { TokenEnd } 
    DEF                { TokenDef }
    INTEGER            { TokenInt } 
    REAL               { TokenReal } 
    BOOLEAN            { TokenBool }
    WHILE              { TokenWhile }
    FOR                { TokenFor }
    IF                 { TokenIf }
    ELSE               { TokenElse }
    AND                { TokenAnd }
    OR                 { TokenOr }
    NOT                { TokenNot }
    XOR                { TokenXor }
    PRINT              { TokenPrint } 
    RETURN             { TokenReturn }

    // Separators
    NEWLINE            { TokenNewline }
    SEMICOLON          { TokenSemicolon }

    // Delimiters
    LPAREN             { TokenLParen }
    RPAREN             { TokenRParen }
    LBRACKET           { TokenLBracket }
    RBRACKET           { TokenRBracket }
    COMMA              { TokenComma }
    DOT                { TokenDot }
    COLON              { TokenColon }
    IN_RANGE           { TokenInRange }

    // Operator signs
    EQUALS             { TokenEquals }
    ASSIGN             { TokenAssign }
    NEQUALS            { TokenNequals}
    GREATER            { TokenGreater }
    LESS               { TokenLess }
    LEQUALS            { TokenLequals }
    GEQUALS            { TokenGequals }
    ADD                { TokenAdd }
    SUB                { TokenSub }
    MUL                { TokenMul }
    DIV                { TokenDiv }
    REM                { TokenRem }

%%

Program
    : /* empty */           { ??? }
    | Statement Program     { $1 }
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
    : MUL               { * }
    | DIV               { / }
    | REM               { % }
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