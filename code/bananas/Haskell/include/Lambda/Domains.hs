{-

(C) 2010, Ralf Laemmel

Syntactical and semantic domains of a CBV interpreter for lambda terms.

We use it as a starting point for going after big bananas.

-}

module Lambda.Domains where

import Data.Map (Map)


-- Names in the sense of lambda variables

type Name = String


-- Lambda terms

data Term =
   Var Name
 | Lambda Name Term
 | Apply Term Term
 | Zero
 | Succ Term
 | Pred Term
 | IsZero Term
 | Cond Term Term Term


-- Interpretation results

data Value
 = InInt Int
 | InBool Bool
 | InFun (Value -> Maybe Value)

instance Show Value where
  show (InInt n)  = show n
  show (InBool b) = show b
  show (InFun _)  = "<function>"


-- Environments 

type Env = Map String Value
