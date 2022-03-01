-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Program.Par
  ( happyError
  , myLexer
  , pProgram
  , pListDecl
  , pListExpr
  , pListIdent
  , pDecl
  , pStatement
  , pExpr6
  , pExpr5
  , pExpr4
  , pExpr3
  , pExpr2
  , pExpr1
  , pExpr
  ) where

import Prelude

import qualified Program.Abs
import Program.Lex

}

%name pProgram Program
%name pListDecl ListDecl
%name pListExpr ListExpr
%name pListIdent ListIdent
%name pDecl Decl
%name pStatement Statement
%name pExpr6 Expr6
%name pExpr5 Expr5
%name pExpr4 Expr4
%name pExpr3 Expr3
%name pExpr2 Expr2
%name pExpr1 Expr1
%name pExpr Expr
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!='     { PT _ (TS _ 1)  }
  '%'      { PT _ (TS _ 2)  }
  '('      { PT _ (TS _ 3)  }
  ')'      { PT _ (TS _ 4)  }
  '*'      { PT _ (TS _ 5)  }
  '+'      { PT _ (TS _ 6)  }
  ','      { PT _ (TS _ 7)  }
  '-'      { PT _ (TS _ 8)  }
  '/'      { PT _ (TS _ 9)  }
  ':'      { PT _ (TS _ 10) }
  ';'      { PT _ (TS _ 11) }
  '<'      { PT _ (TS _ 12) }
  '<='     { PT _ (TS _ 13) }
  '='      { PT _ (TS _ 14) }
  '=='     { PT _ (TS _ 15) }
  '>'      { PT _ (TS _ 16) }
  '>='     { PT _ (TS _ 17) }
  'and'    { PT _ (TS _ 18) }
  'def'    { PT _ (TS _ 19) }
  'else'   { PT _ (TS _ 20) }
  'for'    { PT _ (TS _ 21) }
  'if'     { PT _ (TS _ 22) }
  'in'     { PT _ (TS _ 23) }
  'not'    { PT _ (TS _ 24) }
  'or'     { PT _ (TS _ 25) }
  'range'  { PT _ (TS _ 26) }
  'return' { PT _ (TS _ 27) }
  'while'  { PT _ (TS _ 28) }
  'xor'    { PT _ (TS _ 29) }
  '{'      { PT _ (TS _ 30) }
  '}'      { PT _ (TS _ 31) }
  L_Ident  { PT _ (TV $$)   }
  L_integ  { PT _ (TI $$)   }

%%

Ident :: { Program.Abs.Ident }
Ident  : L_Ident { Program.Abs.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

Program :: { Program.Abs.Program }
Program : ListDecl { Program.Abs.Program $1 }

ListDecl :: { [Program.Abs.Decl] }
ListDecl
  : {- empty -} { [] }
  | Decl { (:[]) $1 }
  | Decl ';' ListDecl { (:) $1 $3 }

ListExpr :: { [Program.Abs.Expr] }
ListExpr
  : {- empty -} { [] }
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }

ListIdent :: { [Program.Abs.Ident] }
ListIdent
  : {- empty -} { [] }
  | Ident { (:[]) $1 }
  | Ident ',' ListIdent { (:) $1 $3 }

Decl :: { Program.Abs.Decl }
Decl
  : 'return' Expr { Program.Abs.DeclReturn $2 }
  | Statement { Program.Abs.DeclStatement $1 }
  | 'def' Ident '(' ListIdent ')' ':' '{' ListDecl '}' { Program.Abs.DeclDef $2 $4 $8 }

Statement :: { Program.Abs.Statement }
Statement
  : Ident '=' Expr { Program.Abs.Assign $1 $3 }
  | Ident '(' ListExpr ')' { Program.Abs.RoutineCall $1 $3 }
  | 'while' '(' Expr ')' ':' '{' ListDecl '}' { Program.Abs.WhileLoop $3 $7 }
  | 'for' Ident 'in' 'range' '(' Expr ',' Expr ')' ':' '{' ListDecl '}' { Program.Abs.ForLoop $2 $6 $8 $12 }
  | 'if' Expr ':' '{' ListDecl '}' { Program.Abs.If $2 $5 }
  | 'if' Expr ':' '{' ListDecl '}' ';' 'else' ':' '{' ListDecl '}' { Program.Abs.IfElse $2 $5 $11 }

Expr6 :: { Program.Abs.Expr }
Expr6
  : Integer { Program.Abs.EInt $1 }
  | Ident { Program.Abs.EVar $1 }
  | Ident '(' ListExpr ')' { Program.Abs.ERCall $1 $3 }

Expr5 :: { Program.Abs.Expr }
Expr5
  : '-' Expr6 { Program.Abs.ENeg $2 }
  | Expr6 { $1 }
  | '(' Expr ')' { $2 }

Expr4 :: { Program.Abs.Expr }
Expr4 : 'not' Expr5 { Program.Abs.ENot $2 } | Expr5 { $1 }

Expr3 :: { Program.Abs.Expr }
Expr3
  : Expr3 '*' Expr4 { Program.Abs.ETimes $1 $3 }
  | Expr3 '/' Expr4 { Program.Abs.EDiv $1 $3 }
  | Expr3 '%' Expr4 { Program.Abs.ERem $1 $3 }
  | Expr4 { $1 }

Expr2 :: { Program.Abs.Expr }
Expr2
  : Expr2 '+' Expr3 { Program.Abs.EPlus $1 $3 }
  | Expr2 '-' Expr3 { Program.Abs.EMinus $1 $3 }
  | Expr3 { $1 }

Expr1 :: { Program.Abs.Expr }
Expr1
  : Expr1 'and' Expr2 { Program.Abs.EAND $1 $3 }
  | Expr1 'or' Expr2 { Program.Abs.EOR $1 $3 }
  | Expr1 'xor' Expr2 { Program.Abs.EXOR $1 $3 }
  | Expr2 { $1 }

Expr :: { Program.Abs.Expr }
Expr
  : Expr '<' Expr1 { Program.Abs.ELess $1 $3 }
  | Expr '>' Expr1 { Program.Abs.EGrt $1 $3 }
  | Expr '<=' Expr1 { Program.Abs.EELess $1 $3 }
  | Expr '>=' Expr1 { Program.Abs.EEGrt $1 $3 }
  | Expr '==' Expr1 { Program.Abs.EEQUAL $1 $3 }
  | Expr '!=' Expr1 { Program.Abs.ENEQUAL $1 $3 }
  | Expr1 { $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

