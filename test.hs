main :: IO ()
main = do 
  handle <- openFile "sum.py" ReadMode 
  input <- hGetContents handle
  let tokens = resolveLayout True (myLexer input)
  case pProgram tokens of
    Left err -> print err
    Right program -> do
      putStrLn "Before:"
      putStrLn (printTree program)
      putStrLn "=============================="
      putStrLn "freeVars:"
      putStrLn (printTree (freeVars program))
      putStrLn "freeAndDeclared:"
      print (freeAndDeclared program)
      putStrLn "Update Routine Decl:"
      putStrLn (printTree (addParams program))
      