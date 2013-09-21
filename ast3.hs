module AST where

type Statements = [Statement]

data Statement = SExpr Expression 
               | Assign String Expression
               deriving Show

data Expression = ETerm Term
                | Plus Expression Expression
                | Minus Expression Expression
                | Eq Expression Expression
                | Lt Expression Expression
                | Gt Expression Expression
                | Not Expression
                | If Expression Expression Expression
                deriving Show

data Term = Num Int
          | Var String
          | TBool Bool
          | Parens Expression
          deriving Show