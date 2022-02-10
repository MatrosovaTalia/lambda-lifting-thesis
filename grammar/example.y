{
module Main where
import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%token 
      let             { TokenLet }
      in              { TokenIn }
      int             { TokenInt $$ }
      var             { TokenVar $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

Exp   : let var '=' Exp in Exp  { \p -> $6 (($2,$4 p):p) }
      | Exp1                    { $1 }

Exp1  : Exp1 '+' Term           { \p -> $1 p + $3 p }
      | Exp1 '-' Term           { \p -> $1 p - $3 p }
      | Term                    { $1 }

Term  : Term '*' Factor         { \p -> $1 p * $3 p }
      | Term '/' Factor         { \p -> $1 p `div` $3 p }
      | Factor                  { $1 }

Factor			  
      : int                     { \p -> $1 }
      | var                     { \p -> case lookup $1 p of
 	                                    Nothing -> error "no var"
 					    Just i  -> i }
      | '(' Exp ')'             { $2 }


-- Exp   : let var '=' Exp in Exp  { \p -> $6 (($2,$4 p):p) }
--       | Exp '+' Exp             { \p -> $1 p + $3 p }
      -- | Exp '-' Exp             { Minus $1 $3 }
      -- | Exp '*' Exp             { Times $1 $3 }
      -- | Exp '/' Exp             { Div $1 $3 }
      -- | '(' Exp ')'             { Brack $2 }
      -- --| '-' Exp %prec NEG       { Negate $2 }
      -- | int                     { Int $1 }
      -- | var                     { Var $1 }

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
      | TokenIn
      | TokenInt Int
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

main = getContents >>= print . flip calc [] . lexer
}