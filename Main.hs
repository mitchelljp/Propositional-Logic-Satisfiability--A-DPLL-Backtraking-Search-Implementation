module Main where

import Inf2d
import System.IO
import Control.Exception
import Data.List

-- Main function
main :: IO ()
main = do
    putStr "\nStudent ID: " 
    putStrLn $ studentId ++ "\n"
    putStrLn "Which section would you like to check?"
    putStrLn "1. TT-ENTAILS"
    putStrLn "2. DPLL"
    putStrLn "3. (Neither, I want to quit this program)"
    getInt >>= (\i -> case i of
                        1 -> runTTEntailsSubMenu
                        2 -> runDpllSubMenu
                        3 -> putStrLn "Ok, goodbye."
                        (_) -> do putStrLn "That wasn't an option ... please try again."
                                  main)
        where getInt = getLine >>= (\str -> ((readIO str)::IO Int)) 
       

runTTEntailsSubMenu :: IO ()
runTTEntailsSubMenu = do
    putStrLn "\nWhich TT-Entails functions would you like to check? "
    putStrLn "1. Get symbols"
    putStrLn "2. Generate Models"
    putStrLn "3. Evaluate logic sentence"
    putStrLn "4. Check if model entails sentence"
    putStrLn "5. Run TT-ENTAILS"
    putStrLn "6. (Neither, I want to quit this program)"
    putStrLn "Enter your option: "
    getInt >>= (\i -> case i of
                        1 -> runGetSymbols
                        2 -> runGenerateModels
                        3 -> runEvaluateLogic
                        4 -> runPLTrue
                        5 -> runTTEntailsFn
                        6 -> putStrLn "Ok, goodbye."
                        (_) -> do putStrLn "That wasn't an option ... please try again."
                                  main)
        where getInt = getLine >>= (\str -> ((readIO str)::IO Int))


--evaluate ttentail functions
runGetSymbols:: IO ()
runGetSymbols = do
    clauses <- getKB " sentences"
    putStrLn $ "\nSymbols: " ++ (show $ getSymbols clauses)

runGenerateModels:: IO ()
runGenerateModels = do
    symbols <- getSymbolsFromConsole
    putStrLn $ "\nModels:\n" ++ (show $ generateModels symbols)

runEvaluateLogic:: IO ()
runEvaluateLogic = do
    stmt <- getSentence " sentence"
    model <- getModelFromConsole
    putStrLn $ "\nResult: " ++ (show $ pLogicEvaluate stmt model)

runPLTrue:: IO ()
runPLTrue = do
    stmts <- getKB " statements"
    model <- getModelFromConsole
    putStrLn $ "\nResult: " ++ (show $ plTrue stmts model)

runTTEntailsFn :: IO ()
runTTEntailsFn = do
    kb <- getKB " knowledge base statements"
    query <- getSentence " query statement"
    putStrLn $"\nResult: " ++ (show $ ttEntails kb query)

--sub menu for DPLL functions
runDpllSubMenu :: IO ()
runDpllSubMenu = do
    putStrLn "\nWhich DPLL functions would you like to check? "
    putStrLn "1. Early termination"
    putStrLn "2. Pure symbol heuristic"
    putStrLn "3. Unit clause heuristic"
    putStrLn "4. DPLL"
    putStrLn "5. Run DPLL satisfiability check"
    putStrLn "6. (Neither, I want to quit this program)"
    putStrLn "Enter your option: "
    getInt >>= (\i -> case i of
                        1 -> runEarlyTerminationFn
                        2 -> runPureSymbolHeuristicFn
                        3 -> runUnitClauseHeuristicFn
                        4 -> runDpllFn
                        5 -> runDpllSatFn
                        6 -> putStrLn "Ok, goodbye."
                        (_) -> do putStrLn "That wasn't an option ... please try again."
                                  main)
        where getInt = getLine >>= (\str -> ((readIO str)::IO Int))

-- evaluate dpll function
runEarlyTerminationFn :: IO ()
runEarlyTerminationFn = do
    sentence <- getSentence " sentence"
    model <- getModelFromConsole
    putStrLn $ "\nResult: " ++ (show $ earlyTerminate sentence model)

runPureSymbolHeuristicFn :: IO ()
runPureSymbolHeuristicFn = do
    symbols <- getSymbolsFromConsole
    clauses <- getSentence " CNF clauses"
    model <- getModelFromConsole
    putStrLn $ "\nPure Symbol: " ++ (show $ findPureSymbol symbols clauses model)

runUnitClauseHeuristicFn :: IO ()
runUnitClauseHeuristicFn = do
    clauses <- getSentence " CNF clauses"
    model <- getModelFromConsole
    putStrLn $ "\nUnit Clause: " ++ (show $ findUnitClause clauses model)

runDpllFn :: IO ()
runDpllFn = do
    clauses <- getSentence " CNF clauses"
    symbols <- getSymbolsFromConsole
    putStrLn $ "\nResult: " ++ (show $ dpll clauses symbols)

runDpllSatFn :: IO ()
runDpllSatFn = do
    clauses <- getSentence " CNF clauses"
    putStrLn $ "\nResult: " ++ (show $ dpllSatisfiable clauses)



-- general helpers for getting inputs from console
getKB :: String -> IO KB
getKB name = do
  putStrLn $ "\nEnter" ++ name ++ " (eg: [ [[\"a\",\"b\"],[\"-b\",\"c\"]], [[\"-a\"],[\"-b\",\"c\"]] ]) :"
  str <- getLine
  stmt <- catch((readIO str)::IO KB)
        (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException))
                  getKB name)
  return stmt

getSentence :: String -> IO Sentence
getSentence name = do
  putStrLn $ "\nEnter" ++ name ++ " (eg: [[\"a\",\"b\"],[\"-b\",\"c\"]]) :"
  str <- getLine
  stmt <- catch((readIO str)::IO Sentence)
        (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException))
                  getSentence name)
  return stmt

getSymbolsFromConsole :: IO [Symbol]
getSymbolsFromConsole = do
  putStrLn $ "\nEnter symbols (eg: [\"a\",\"b\",\"c\"]) :"
  str <- getLine
  stmt <- catch((readIO str)::IO [Symbol])
        (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException))
                  getSymbolsFromConsole)
  return stmt

getModelFromConsole :: IO Model
getModelFromConsole = do
  putStrLn $ "\nEnter a model (eg:  [(\"a\",True),(\"b\",False),(\"c\",True)] ) :"
  str <- getLine
  stmt <- catch((readIO str)::IO Model)
        (\e -> do putStrLn ("**Error: " ++ show (e :: SomeException))
                  getModelFromConsole)
  return stmt