module AST where

type Statements = [Statement]

data Statement = SExpr Expression 
               | Assign String Expression
               deriving Show

data Expression = ETerm Term
                | Plus Expression Expression
                | Minus Expression Expression
                deriving Show

data Term = Num Int
          | Var String
          deriving Show