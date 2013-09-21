module SimParser where

import Text.ParserCombinators.Parsec

data Statement = SExpr Expression deriving (Show)
data Expression = ETerm Term
                | Plus Term Expression
                | Minus Term Expression
                deriving (Show)
data Term = Num Int deriving (Show)

parseInt :: Parser Int
parseInt = spaces >> (fmap read $ many1 digit)

parseTerm :: Parser Term
parseTerm = spaces >> fmap Num parseInt

parseExpression :: Parser Expression
parseExpression = spaces >> do 
  t <- parseTerm
  sym <- (spaces >> optionMaybe (oneOf ['+', '-']))
  case sym of
    Nothing -> return $ ETerm t
    Just '+' -> fmap (Plus t) parseExpression
    Just '-' -> fmap (Minus t) parseExpression

parseStatement :: Parser Statement
parseStatement = spaces >> do
  e <- parseExpression
  spaces >> char ';'
  return (SExpr e)

run input = case parse parseStatement "sim" input of
  Right val -> "Found value: " ++ show val
  Left err -> "Parse error: " ++ show err

runParse = putStrLn . run