module Main where

import Parser
import Evaluator
import Types

main :: IO ()
main = do
    putStr "insert the expression: "
    input <- getLine
    case readExpr input of 
        Left err   -> print err 
        Right expr -> printout $ evaluate expr

    where 
        evaluate expr = runEval $ eval expr
        printout ((Left DivideByZero, C n), Log log) = do
            putStrLn "Error: divide by zero"
            putStrLn $ "evaluated expression: " ++ show n
            putStrLn $ "Log:\n" ++ log
        printout ((Right value, C n), Log log) = do
            putStrLn $ "Result: " ++ show value
            putStrLn $ "evaluated expression: " ++ show n
            putStrLn $ "Log:\n" ++ log