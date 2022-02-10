{
module Main where
import Data.Char
}

%name toygrammar
%tokentype { Token }
%error { parseError }


-- %nonassoc EQUALS LEQUALS LESS GEQUALS GREATER NEQUALS
%left PLUS MINUS
%left MUL DIV REM
%left AND OR XOR
-- %left NOT 




%token  
    -- Identifiers & numbers
    IDENTIFIER         { TokenId }
    INTEGER_LITERAL    { TokenIntLit $$ }
    REAL_LITERAL       { TokenRealLit $$ }
    TRUE               { TokenTrue }
    FALSE              { TokenFalse }

    -- Keywords
    END                { TokenEnd } 
    DEF                { TokenDef }
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
    --INTEGER            { TokenInt } 
    --REAL               { TokenReal } 
    --BOOLEAN            { TokenBool }

    -- Separators
    NEWLINE            { TokenNewline }

    -- Delimiters
    LPAREN             { TokenLParen }
    RPAREN             { TokenRParen }
    LBRACKET           { TokenLBracket }
    RBRACKET           { TokenRBracket }
    COMMA              { TokenComma }
    COLON              { TokenColon }
    IN_RANGE           { TokenInRange }

    -- Operator signs
    EQUALS             { TokenEquals }
    PLUS               { TokenAdd }
    MINUS              { TokenSub }
    MUL                { TokenMul }
    DIV                { TokenDiv }
    REM                { TokenRem }
    -- ASSIGN             { TokenAssign }
    -- NEQUALS            { TokenNequals}
    -- GREATER            { TokenGreater }
    -- LESS               { TokenLess }
    -- LEQUALS            { TokenLequals }
    -- GEQUALS            { TokenGequals }

%%

Program    
    : {}
    | Statement Program   {}
    ;

RoutineDeclaration 
    : DEF Identifier LPAREN Parameters RPAREN Body END      {}
    | DEF Identifier LPAREN Parameters RPAREN COLON Body END {}


Parameters
    : {} --/* empty */ {}
    | Identifier COMMA Parameters {}
    ;

Array
    : LBRACKET Primaries RBRACKET  {}
    ;

ArrayElement
    : Identifier LBRACKET Expression RBRACKET {}
    ;

Primaries
    : Primary {}
    | COMMA Primaries {}
    ;

Body
    : {}--/* empty */ 
    | BodyDeclaration Body{}
    ;

BodyDeclaration
    : Return {}
    | Statement{}
    ;

Statement
    : Assignment  {}
    | RoutineCall  {}
    | WhileLoop  {}
    | ForLoop  {}
    | IfStatement {}
    | Print  {}
    | RoutineDeclaration  {}
    ;

Return
    : RETURN Expression {}
    ;

Assignment
    : Identifier EQUALS Expression {}
    ;

RoutineCall
    : Identifier LPAREN Expressions RPAREN {}
    ;

Expressions
    : Expression {}
    | Expression COMMA Expressions {}
    ;

WhileLoop
    : WHILE Expression COLON Body END {}
    ;

ForLoop
    : FOR Identifier IN_RANGE LPAREN Expression COMMA Expression RPAREN COLON NEWLINE Body END {}
    ;

IfStatement
    : IF Expression COLON NEWLINE Body END {}
    | IF Expression COLON NEWLINE Body ELSE Body END {}
    ;


Expression
    : Expression PLUS Expression  { \p -> $1 p + $3 p}
    | Expression MINUS Expression {}
    | Expression MUL Expression   {}
    | Expression DIV Expression   {}
    | Expression REM Expression   {}
    | Expression AND Expression   {}
    | Expression OR Expression    {}
    | Expression XOR Expression   {}
    | LPAREN Expression RPAREN    {}
    --| NOT Expression              {}
    | Primary {}
    ;

Primary
    : INTEGER_LITERAL { \p -> $1 }
    | REAL_LITERAL    {}
    | TRUE            {}
    | FALSE           {}
    | RoutineCall     {}
    | ArrayElement    {}
    | Array           {}
    ;

Print
    : PRINT LPAREN Expressions RPAREN {}
    ;

Identifier:
	IDENTIFIER {}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
data Exp  
    = Let String Exp Exp
    | Exp1 Exp1
    deriving Show

data Exp1 
    = Plus Exp1 Term 
    | Minus Exp1 Term 
    | Term Term
    deriving Show

data Term 
    = Times Term Factor 
    | Div Term Factor 
    | Factor Factor
    deriving Show

data Factor 
    = Int Int 
    | Var String 
    | Brack Exp
    deriving Show

data Token
    = TokenLet
    | TokenIntLit Int
    | TokenVar String
    | TokenRealLit Float
    | TokenTrue 
    | TokenFalse 
    | TokenEquals 
    | TokenAdd 
    | TokenSub 
    | TokenMul
    | TokenDiv 
    | TokenRem
    | TokenLParen 
    | TokenRParen 
    | TokenLBracket 
    | TokenRBracket 
    | TokenComma 
    | TokenColon 
    | TokenInRange
    | TokenNewline 
    | TokenEnd  
    | TokenDef 
    | TokenWhile 
    | TokenFor 
    | TokenIf 
    | TokenElse 
    | TokenAnd 
    | TokenOr 
    | TokenNot 
    | TokenXor 
    | TokenPrint  
    | TokenReturn
    | TokenId 
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    --   | isSpace c = lexer cs
    --   | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer (EQUALS:cs) = TokenEq : lexer cs
lexer (PLUS:cs)   = TokenPlus : lexer cs
lexer (MINUS:cs)  = TokenMinus : lexer cs
lexer (MUL:cs)    = TokenTimes : lexer cs
lexer (DIV:cs)    = TokenDiv : lexer cs
lexer (LPAREN:cs) = TokenLParen : lexer cs
lexer (RPAREN:cs) = TokenRParen : lexer cs

lexNum cs = TokenIntLit (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

main = getContents >>= print . flip calc [] . lexer
}