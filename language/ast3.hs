module AST where

type Statements = [Statement]

data Statement = SExpr Expression 
               | Assign String Expression
               deriving Show

data Expression = ETerm Term
                | Binary String Expression Expression
                | Not Expression
                | If Expression Expression Expression
                deriving Show

data Term = Num Int
          | Var String
          | TBool Bool
          | Parens Expression
          deriving Show