module Main where

import           Ast.Abs
import           Ast.Layout (resolveLayout)
import           Ast.Par    (myLexer, pAst)
import           Ast.Print  (printTree, Print)
import           Data.List      ((\\), isPrefixOf, elemIndices, partition, nub)
import           Data.Maybe     (listToMaybe)
import           Text.Show
import           System.IO
import           Control.Monad
import Text.XHtml (body)
import Foreign (free)
import Control.Monad.State (State)
-- import           Data.Map


-- stage1: generate recursive AST
generateRecursiveAST :: Ast -> rAst
treeToVectorTree :: rAst -> VectorTree
treeToVectorTree tree = (depthAcc, typesAcc, valuesAcc)
    where
        treeToList currLevel (Tree root children)
            = (currLevel, nodeType root, nodeValue root)
            : P.concatMap (treeToList (currLevel + 1)) children

        listTree = treeToList 0 tree

        depthVector = P.map (\(d, _, _) -> d) listTree
        types  = padToEqualLength '\0' $ P.map (\(_, t, _) -> t) listTree
        values = padToEqualLength '\0' $ P.map (\(_, _, v) -> v) listTree

        maxLength arr = P.maximum (P.map P.length arr)

        depthAcc = fromList (Z :. P.length depthVector) depthVector
        typesAcc = fromList (Z :. P.length types :. maxLength types) (P.concat types)
        valuesAcc = fromList (Z :. P.length values :. maxLength values) (P.concat values)



-- stage2: calculate scopes number -> list on Nodes (scopes, de Bruijn ids, depth, value , type)
getScopesCount :: rAst -> [(Ident, Int)]

-- -- stage3: depth scopes
-- getDeBruijn :: rAst -> [(Ident, Int)]

--stage4: Construct PrAst
buildParrallelAST :: rAst -> PAST

--stage5: lift
liftPast :: PAST -> PAST

--stage6: convert PAST to rAst
convertToAST :: PAST -> rAst

printResult :: rAst -> Text