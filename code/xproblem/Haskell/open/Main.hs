module Main where

-- Library
import ColonT

-- Expressions
import Data
import Evaluator
import PrettyPrinter
import ToTree
import Negation
import Eq
import Show
import ShowType
import ToTree

main = do
          -- Sample terms
          let e1 = Const 17
          let e2 = Const 25
          let e3 = Add e1 e2

          -- Computations
          putStr $ "show:         " ++ show e3 ++ "\n"
          putStr $ "toTree:       " ++ show (toTree e3) ++ "\n"
          putStr $ "showType:     " ++ showType e3 ++ "\n"
          putStr $ "evaluate:     " ++ show (evaluate e3) ++ "\n"
          putStr   "prettyPrint:  " >> prettyPrint e3 >> putStrLn ""
          putStr $ "eq:           " ++ show (eq2 e1 e3) ++ "\n"
