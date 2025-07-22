{-# LANGUAGE ScopedTypeVariables #-}

import Language.Haskell.Interpreter
import ElementAdder
import MyModule
import MyModule2

main :: IO ()
main = do
  -- Define the module and function names as strings
  let moduleName = "ElementAdder"
      functionName = "oneAdd"

  -- Run the Haskell module using the GHC interpreter
  result <- runInterpreter $ do
    setImports ["Prelude", moduleName]
    setTopLevelModules [moduleName]
    loadModules [moduleName ++ ".hs"]
    setImportsQ [(moduleName, Just moduleName)]
    interpret (functionName ++ " [1,2,3,4,5]") (as :: [Int])

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x

{-# LANGUAGE ScopedTypeVariables #-}

--import Language.Haskell.Interpreter

main1 :: IO ()
main1 = do
  -- Define the list as a string and insert it into a Haskell module
  let listStr = "[1,2,3,4,5]" :: String
      moduleStr = "module MyModule where\n\nmyList = " ++ listStr ++ "\n\naddToList x = x : myList"

  -- Run the Haskell module using the GHC interpreter
  result <- runInterpreter $ do
    setImports ["Prelude"]
    setTopLevelModules ["MyModule"]
    loadModules ["MyModule.hs"]
    setImportsQ [("MyModule", Just "MyModule")]
    interpret "addToList 6" (as :: [Int])

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x


main3 :: IO ()
main3 = do
  -- Define the Haskell code as a string
  let haskellCode = "module MyModule3 where\n\nfactorial :: Int -> Int\nfactorial 0 = 1\nfactorial n = n * factorial (n-1)"

  -- Run the Haskell code using the GHC interpreter
  result <- runInterpreter $ do
    setImports ["Prelude"]
    setTopLevelModules ["MyModule3"]
    loadModules ["MyModule3.hs"]
    setImportsQ [("MyModule3", Just "MyModule3")]
    interpret "factorial 5" (as :: Int)

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x

--theAdder :: [Int] -> [Int]
--theAdder x = x ++ [1]

main4 :: IO ()
main4 = do
  -- Define the Haskell expression and its input as strings
  let exprStr = "oneAdd"
      inputStr = "[1,2,3]"

  -- Evaluate the expression with the input using hint
  result <- runInterpreter $ do
    setImports ["ElementAdder"]
    expr <- interpret exprStr (as :: [Int] -> [Int])
    input <- interpret inputStr (as :: [Int])
    return $ expr input

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x


myAdd2 :: [Int] -> [Int]
myAdd2 xs = xs ++ [1]

main5 :: IO ()
main5 = do
  let expStr = "myAdd2 [2,3,4]"
      moduleStr = "module MyModule where\n\nimport Main\n\n"

  result <- runInterpreter $ do
    setImports ["Prelude"]
    setTopLevelModules ["MyModule"]
    loadModules ["Main.hs"]
    interpret moduleStr (as :: Interpreter ())
    setImportsQ [("MyModule", Nothing)]
    interpret expStr (as :: [Int])

  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x


--import Language.Haskell.Interpreter

main6 :: IO ()
main6 = do
  -- Define the Haskell expression and its input as strings
  let exprStr = "factorial"
      inputStr = "5"

  -- Evaluate the expression with the input using hint
  result <- runInterpreter $ do
    setImports ["Prelude"]
    set [languageExtensions := [RecursiveDo]]
    loadModules ["MyModule2.hs"]
    expr <- interpret exprStr (as :: Int -> Int)
    input <- interpret inputStr (as :: Int)
    return $ expr input

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x

-- Define the factorial function in a separate module
--module MyModule where

--factorial :: Int -> Int
--factorial 0 = 1
--factorial n = n * factorial (n - 1)

myFunction :: Int -> Int
myFunction x = x * 2

main7 :: IO ()
main7 = do
  -- Define the Haskell expression and its input as strings
  let exprStr = "map (myFunction)"
      inputStr = "[1,2,3]"

  -- Evaluate the expression with the input using hint
  result <- runInterpreter $ do
    setImports ["Prelude"]
    setImportsQ [("Main", Nothing)]
    setTopLevelModules ["Main"]
    expr <- interpret exprStr (as :: [Int] -> [Int])
    input <- interpret inputStr (as :: [Int])
    return $ expr input

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x

--In this example, we define a function called myFunction that multiplies an integer by 2. We then use this function in the exprStr string by calling it with map. We also need to import the Main module and set it as a top-level module using setImportsQ and setTopLevelModules, respectively.






