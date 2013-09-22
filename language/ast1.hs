module AST where

data Statement = SExpr Expression deriving Show

data Expression = ETerm Term
                | Plus Expression Expression
                | Minus Expression Expression
                deriving Show
                
data Term = Num Int deriving Show