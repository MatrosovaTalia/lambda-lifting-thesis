module Main where

import           Ast.Abs
import           Ast.Layout          (resolveLayout)
import           Ast.Par             (myLexer, pAst)
import           Ast.Print           (Print, printTree)
import           Control.Monad
import           Control.Monad.State (State)
import           Data.List           (elemIndices, isPrefixOf, nub, partition,
                                      (\\))
import           Data.Maybe          (listToMaybe)
import           Foreign             (free)
import           System.IO
import           Text.Show
-- import           Data.Map
import           ScopesParallel
import qualified Data.Array.Accelerate.LLVM.Native as Acc



main :: IO ()
main = do
  handle <- openFile "tests/sum.py" ReadMode
  input <- hGetContents handle
  let tokens = resolveLayout True (myLexer input)
  case pAst tokens of
    Left err -> print err
    Right ast@(Ast decls) -> do
      let entries = blockToEntry emptyContext decls
          nodesWithDepth = (\e -> (entryDepth e, entryNodeType e)) <$> entries
      putStr $ ppNodesWithDepth nodesWithDepth
      let past = toPAST ast
      putStr (ppPAST past)
      print (Acc.run (findNodesOfType isDef past))
      -- print (Acc.run (findAncestorsOfType isDef past))
  
  

-- treeToVectorTree :: rAst -> VectorTree
-- treeToVectorTree tree = (depthAcc, typesAcc, valuesAcc)
--     where
--         treeToList currLevel (Tree root children)
--             = (currLevel, nodeType root, nodeValue root)
--             : P.concatMap (treeToList (currLevel + 1)) children
--
--         listTree = treeToList 0 tree
--
--         depthVector = P.map (\(d, _, _) -> d) listTree
--         types  = padToEqualLength '\0' $ P.map (\(_, t, _) -> t) listTree
--         values = padToEqualLength '\0' $ P.map (\(_, _, v) -> v) listTree
--
--         maxLength arr = P.maximum (P.map P.length arr)
--
--         depthAcc = fromList (Z :. P.length depthVector) depthVector
--         typesAcc = fromList (Z :. P.length types :. maxLength types) (P.concat types)
--         valuesAcc = fromList (Z :. P.length values :. maxLength values) (P.concat values)
--
-- -- stage2: calculate scopes number -> list on Nodes (scopes, de Bruijn ids, depth, value , type)
-- getScopesCount :: rAst -> [(Ident, Int)]
--
-- -- -- stage3: depth scopes
-- -- getDeBruijn :: rAst -> [(Ident, Int)]
--
-- --stage4: Construct PrAst
-- buildParrallelAST :: rAst -> PAST
--
-- --stage5: lift
-- liftPast :: PAST -> PAST
--
-- --stage6: convert PAST to rAst
-- convertToAST :: PAST -> rAst
--
-- printResult :: rAst -> Text
