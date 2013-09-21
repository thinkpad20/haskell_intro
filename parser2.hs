module SimParser where

import Text.ParserCombinators.Parsec

data Statement = SExpr Expression 
               | Assign String Expression
               deriving Show

data Expression = ETerm Term
                | Plus Term Expression
                | Minus Term Expression
                deriving Show

data Term = Num Int
          | Var String
          | Parens Expression
          deriving Show

parseInt :: Parser Int
parseInt = spaces >> (fmap read $ many1 digit)

parseId :: Parser String
parseId = spaces >> many1 letter

parseTerm :: Parser Term
parseTerm = fmap Num parseInt <|> fmap Var parseId

parseParens :: Parser Expression
parseParens = do
  spaces >> char '(' >> spaces
  e <- parseExpression
  spaces >> char ')'
  return e

parseExpression :: Parser Expression
parseExpression = getTerm <|> parseParens where
  getTerm = do
    t <- parseTerm
    spaces
    sym <- optionMaybe (oneOf ['+', '-'])
    case sym of
      Nothing -> return $ ETerm t
      Just '+' -> fmap (Plus t) parseExpression
      Just '-' -> fmap (Minus t) parseExpression

parseAssign :: Parser Statement
parseAssign = spaces >> do
  var <- parseId
  spaces >> char '=' >> spaces
  e <- parseExpression
  spaces
  char ';'
  return (Assign var e)

parseStatement :: Parser Statement
parseStatement = try parseAssign
  <|> do
    e <- parseExpression
    spaces >> char ';'
    return (SExpr e)

run input = case parse parseStatement "sim" input of
  Right val -> "Found value: " ++ show val
  Left err -> "Parse error: " ++ show err

runParse = putStrLn . run