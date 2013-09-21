module Parser where

import Text.ParserCombinators.Parsec
import AST

parseInt :: Parser Int
parseInt = spaces >> (fmap read $ many1 digit)

parseTerm :: Parser Term
parseTerm = spaces >> fmap Num parseInt

parseExpression :: Parser Expression
parseExpression = spaces >> do 
  term <- parseTerm
  let t = ETerm term
  sym <- (spaces >> optionMaybe (oneOf ['+', '-']))
  case sym of
    Nothing -> return $ t
    Just '+' -> fmap (Plus t) parseExpression
    Just '-' -> fmap (Minus t) parseExpression

parseStatement :: Parser Statement
parseStatement = spaces >> do
  e <- parseExpression
  spaces >> char ';'
  return (SExpr e)

getParse :: String -> Either ParseError Statement
getParse = parse parseStatement "sim"

run input = case getParse input of
  Right val -> "Found value: " ++ show val
  Left err -> "Parse error: " ++ show err

printParse = putStrLn . run