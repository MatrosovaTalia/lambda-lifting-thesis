-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Ast.

module Ast.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Ast.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Ast.Abs.Ident where
  prt _ (Ast.Abs.Ident i) = doc $ showString i
instance Print Ast.Abs.Ast where
  prt i = \case
    Ast.Abs.Ast decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print [Ast.Abs.Decl] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Ast.Abs.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Ast.Abs.Ident] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Ast.Abs.RoutineDecl where
  prt i = \case
    Ast.Abs.RoutineDecl id_ ids decls -> prPrec i 0 (concatD [doc (showString "def"), prt 0 id_, doc (showString "("), prt 0 ids, doc (showString ")"), doc (showString ":"), doc (showString "{"), prt 0 decls, doc (showString "}")])

instance Print Ast.Abs.Decl where
  prt i = \case
    Ast.Abs.DeclReturn expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr])
    Ast.Abs.DeclStatement statement -> prPrec i 0 (concatD [prt 0 statement])
    Ast.Abs.DeclDef routinedecl -> prPrec i 0 (concatD [prt 0 routinedecl])

instance Print Ast.Abs.Statement where
  prt i = \case
    Ast.Abs.Assign id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])
    Ast.Abs.RoutineCall id_ exprs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Ast.Abs.WhileLoop expr decls -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString ":"), doc (showString "{"), prt 0 decls, doc (showString "}")])
    Ast.Abs.ForLoop id_ expr1 expr2 decls -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id_, doc (showString "in"), doc (showString "range"), doc (showString "("), prt 0 expr1, doc (showString ","), prt 0 expr2, doc (showString ")"), doc (showString ":"), doc (showString "{"), prt 0 decls, doc (showString "}")])
    Ast.Abs.If expr decls -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString ":"), doc (showString "{"), prt 0 decls, doc (showString "}")])
    Ast.Abs.IfElse expr decls1 decls2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString ":"), doc (showString "{"), prt 0 decls1, doc (showString "}"), doc (showString ";"), doc (showString "else"), doc (showString ":"), doc (showString "{"), prt 0 decls2, doc (showString "}")])

instance Print Ast.Abs.Expr where
  prt i = \case
    Ast.Abs.EInt n -> prPrec i 6 (concatD [prt 0 n])
    Ast.Abs.EVar id_ -> prPrec i 6 (concatD [prt 0 id_])
    Ast.Abs.ERCall id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Ast.Abs.ENeg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Ast.Abs.ENot expr -> prPrec i 4 (concatD [doc (showString "not"), prt 5 expr])
    Ast.Abs.ETimes expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "*"), prt 4 expr2])
    Ast.Abs.EDiv expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "/"), prt 4 expr2])
    Ast.Abs.ERem expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "%"), prt 4 expr2])
    Ast.Abs.EPlus expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "+"), prt 3 expr2])
    Ast.Abs.EMinus expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "-"), prt 3 expr2])
    Ast.Abs.EAND expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "and"), prt 2 expr2])
    Ast.Abs.EOR expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "or"), prt 2 expr2])
    Ast.Abs.EXOR expr1 expr2 -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "xor"), prt 2 expr2])
    Ast.Abs.ELess expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "<"), prt 1 expr2])
    Ast.Abs.EGrt expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString ">"), prt 1 expr2])
    Ast.Abs.EELess expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "<="), prt 1 expr2])
    Ast.Abs.EEGrt expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString ">="), prt 1 expr2])
    Ast.Abs.EEQUAL expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "=="), prt 1 expr2])
    Ast.Abs.ENEQUAL expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "!="), prt 1 expr2])
