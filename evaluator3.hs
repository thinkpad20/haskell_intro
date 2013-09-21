module Evaluator where

import AST
import Parser (getParse)
import qualified Data.Map as M

data Value = VNum Int 
           | VBool Bool
           deriving Show

type Env = M.Map String Value
type Result = (Value, Env)

evalSs :: Result -> Statements -> Result
evalSs (v, e) [] = (v, e)
evalSs (v, e) (s:ss) = evalSs (v', e') ss where
  (v', e') = evalS e s

evalS :: Env -> Statement -> Result
evalS env (SExpr e) = (evalE env e, env)
evalS env (Assign name e) = 
  let val = evalE env e in
  (val, M.insert name val env)

evalE :: Env -> Expression -> Value
evalE env (ETerm t)  = evalT env t
evalE env (Plus e1 e2) = VNum $ n1 + n2 where
  VNum n1 = evalE env e1
  VNum n2 = evalE env e2
evalE env (Minus e1 e2) = VNum $ n1 - n2 where
  VNum n1 = evalE env e1
  VNum n2 = evalE env e2
evalE env (Lt e1 e2) = VBool $ n1 < n2 where
  VNum n1 = evalE env e1
  VNum n2 = evalE env e2
evalE env (Gt e1 e2) = VBool $ n1 > n2 where
  VNum n1 = evalE env e1
  VNum n2 = evalE env e2
evalE env (Eq e1 e2) = VBool $ n1 == n2 where
  VNum n1 = evalE env e1
  VNum n2 = evalE env e2
evalE env (Not e) = VBool (not b) where
  VBool b = evalE env e
evalE env (If c t f) = if b then evalE env t else evalE env f where
  VBool b = evalE env c

evalT :: Env -> Term -> Value
evalT env (Num n) = VNum n
evalT env (TBool b) = VBool b
evalT env (Var var) = v where
  res = M.lookup var env
  v = case res of
    Just val -> val
    Nothing -> error $ "Lookup of " ++ var ++ " failed"

eval :: String -> Result
eval input = case getParse input of 
  Right stmts -> evalSs (VNum 0, M.empty) stmts
  Left err -> error $ "Parse error: " ++ show err