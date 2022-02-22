module Examples
    where

import ToypyData

exSimpleSum :: Def
exSimpleSum = Def "sum_2" ["x"]
            [Return (Plus (Term (BoolExp (Factor (Var "x")))) (BoolExp (Factor (Var "n"))))]


exSum :: Def
exSum =
    Def "sum" ["n"]
        [Statement (IfStatement
                (Term (BoolExp (Equals (Factor (Var "n")) (Int 1))))
                [Return (Term (BoolExp (Factor (Int 1))))]

                (Just 
                    [Statement 
                        (RoutineDeclaration 
                            (Def "sum_2" ["x"]
                                [Return (Plus (Term (BoolExp (Factor (Var "x")))) (BoolExp (Factor (Var "n"))))])),
                    Return 
                        (Term (BoolExp (Factor 
                            (RoutineCall "sum_2" 
                                [Term (BoolExp (Factor 
                                    (RoutineCall "sum" 
                                        [Minus (Term (BoolExp (Factor (Var "n")))) (BoolExp (Factor (Int 1)))]) ))]))))
                ])
                    
        )]


examples :: [Def]
examples = [exSimpleSum, exSum]


main :: IO()

main = print (map freeVarsDef examples)
