module Examples
    where

import ToypyData

ex_sum :: Def
ex_sum = Def "sum_2" ["x"]
            [(Return (Plus (Term (BoolExp (Factor (Var "x")))) (BoolExp (Factor (Var "n")))))]

    
-- ex1_sum = 
--     Def "sum" ["n"]
--         [(Statement (IfStatement  
--                 (Term (BoolExp (Equals (Factor (Var "n")) (Int 1))))
--                 [Return (Term (BoolExp (Factor (Int 1))))]
            
--                 [Def "sum_2" ["x"]
--                     [(Return (Plus (Term (Factor (Var "x"))) (Factor (Var "n"))))]

--                 Return (Term (BoolExp (Factor (
--                     RoutineCall "sum_2" 
--                         [Term (BoolExp (Factor ((RoutineCall "sum" 
--                                 [Minus (Term (BoolExp (Factor (Var "n")))) (BoolExp (Factor (Int 1)))]))))]))))
--                 ]
--         ))]



main :: IO()
-- examples = [ex1_sum, ex2, ex3_isqrt, ex7_no_lifting]
main = print (freeVarsDef ex_sum)