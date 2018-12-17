module Main where

-- Expressions
import Data
import Evaluator
import PrettyPrinter
import Negation

main = do
          -- Sample terms
          let e1 = Const 17
          let e2 = Const 25
          let e3 = Add e1 e2

          -- Computations
          putStr $ "evaluate:     " ++ show (evaluate e3) ++ "\n"
          putStr   "prettyPrint:  " >> prettyPrint e3 >> putStrLn ""
