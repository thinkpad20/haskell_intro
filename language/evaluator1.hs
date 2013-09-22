module Evaluator where

import AST
import Parser (getParse)

data Value = VNum Int deriving Show

evalS :: Statement -> Value
evalS (SExpr e) = evalE e

evalE :: Expression -> Value
evalE (ETerm t)  = evalT t
evalE (Plus e1 e2) = 
  let 
    VNum n1 = evalE e1
    VNum n2 = evalE e2
  in
  VNum $ n1 + n2
evalE (Minus e1 e2) = 
  let 
    VNum n1 = evalE e1
    VNum n2 = evalE e2
  in
  VNum $ n1 - n2

evalT :: Term -> Value
evalT (Num n) = VNum n

eval input = case getParse input of 
  Right stmt -> show $ evalS stmt
  Left err -> "Parse error: " ++ show err