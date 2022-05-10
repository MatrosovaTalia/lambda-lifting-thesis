-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Ast.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Ast.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Ast.Abs.Ident -> Result
transIdent x = case x of
  Ast.Abs.Ident string -> failure x

transAst :: Ast.Abs.Ast -> Result
transAst x = case x of
  Ast.Abs.Ast decls -> failure x

transRoutineDecl :: Ast.Abs.RoutineDecl -> Result
transRoutineDecl x = case x of
  Ast.Abs.RoutineDecl ident idents decls -> failure x

transDecl :: Ast.Abs.Decl -> Result
transDecl x = case x of
  Ast.Abs.DeclReturn expr -> failure x
  Ast.Abs.DeclStatement statement -> failure x
  Ast.Abs.DeclDef routinedecl -> failure x

transStatement :: Ast.Abs.Statement -> Result
transStatement x = case x of
  Ast.Abs.Assign ident expr -> failure x
  Ast.Abs.RoutineCall ident exprs -> failure x
  Ast.Abs.WhileLoop expr decls -> failure x
  Ast.Abs.ForLoop ident expr1 expr2 decls -> failure x
  Ast.Abs.If expr decls -> failure x
  Ast.Abs.IfElse expr decls1 decls2 -> failure x

transExpr :: Ast.Abs.Expr -> Result
transExpr x = case x of
  Ast.Abs.EInt integer -> failure x
  Ast.Abs.EVar ident -> failure x
  Ast.Abs.ERCall ident exprs -> failure x
  Ast.Abs.ENeg expr -> failure x
  Ast.Abs.ENot expr -> failure x
  Ast.Abs.ETimes expr1 expr2 -> failure x
  Ast.Abs.EDiv expr1 expr2 -> failure x
  Ast.Abs.ERem expr1 expr2 -> failure x
  Ast.Abs.EPlus expr1 expr2 -> failure x
  Ast.Abs.EMinus expr1 expr2 -> failure x
  Ast.Abs.EAND expr1 expr2 -> failure x
  Ast.Abs.EOR expr1 expr2 -> failure x
  Ast.Abs.EXOR expr1 expr2 -> failure x
  Ast.Abs.ELess expr1 expr2 -> failure x
  Ast.Abs.EGrt expr1 expr2 -> failure x
  Ast.Abs.EELess expr1 expr2 -> failure x
  Ast.Abs.EEGrt expr1 expr2 -> failure x
  Ast.Abs.EEQUAL expr1 expr2 -> failure x
  Ast.Abs.ENEQUAL expr1 expr2 -> failure x
