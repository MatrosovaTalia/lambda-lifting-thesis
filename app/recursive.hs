-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Main where

import           Ast.Abs
import           Ast.Layout          (resolveLayout)
import           Ast.Par             (myLexer, pAst)
import           Ast.Print           (Print, printTree)
import           Data.List           (elemIndices, isPrefixOf, nub, partition,
                                      (\\))
-- import           Data.String
import           Control.Monad
import           Control.Monad.State (State)
import           Data.Maybe          (listToMaybe)
import           Foreign             (free)
import           System.IO
import           Text.Show
-- import           Data.Map

import           Recursive


main :: IO ()
main = do
  handle <- openFile "tests/sum.py" ReadMode
  input <- hGetContents handle
  let tokens = resolveLayout True (myLexer input)
  case pAst tokens of
    Left err -> print err
    Right ast -> do
      putStrLn "Before:"
      putStrLn (printTree ast)
      putStrLn "=============================="
      putStrLn "freeVars:"
      putStrLn (printTree (freeVars ast))
      putStrLn "freeAndDeclared:"
      print (freeAndDeclared ast)
      putStrLn "Rename all bound variables:"
      putStrLn (printTree (renameAst ast))
      putStrLn "Update Routine Decl:"
      putStrLn (printTree (addParams (renameAst ast)))
