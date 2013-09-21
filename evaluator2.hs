module Evaluator where

import AST
import Parser (getParse)
import qualified Data.Map as M

data Value = VNum Int deriving Show
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
evalE env (Plus e1 e2) = 
  let 
    VNum n1 = evalE env e1
    VNum n2 = evalE env e2
  in
  VNum $ n1 + n2
evalE env (Minus e1 e2) = 
  let 
    VNum n1 = evalE env e1
    VNum n2 = evalE env e2
  in
  VNum $ n1 - n2

evalT :: Env -> Term -> Value
evalT env (Num n) = VNum n
evalT env (Var var) = 
  let Just v = M.lookup var env in
  v

eval :: String -> Result
eval input = case getParse input of 
  Right stmts -> evalSs (VNum 0, M.empty) stmts
  Left err -> error $ "Parse error: " ++ show err