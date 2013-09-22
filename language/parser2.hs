module Parser where

import Text.ParserCombinators.Parsec
import AST

schar c = spaces >> char c >> spaces
sstring s = spaces >> string s >> spaces

parseInt :: Parser Int
parseInt = spaces >> (fmap read $ many1 digit)

parseId :: Parser String
parseId = spaces >> many1 letter

parseTerm :: Parser Term
parseTerm = fmap Num parseInt <|> fmap Var parseId

parseParens :: Parser Expression
parseParens = do
  schar '('
  e <- parseExpression
  schar ')'
  return e

parseExpression :: Parser Expression
parseExpression = getTerm <|> parseParens where
  getTerm = do
    term <- parseTerm
    let t = ETerm term
    sym <- (spaces >> optionMaybe (oneOf ['+', '-']))
    case sym of
      Nothing -> return t
      Just '+' -> fmap (Plus t) parseExpression
      Just '-' -> fmap (Minus t) parseExpression

parseAssign :: Parser Statement
parseAssign = spaces >> do
  var <- parseId
  schar '='
  e <- parseExpression
  schar ';'
  return (Assign var e)

parseStatement :: Parser Statement
parseStatement = try parseAssign
  <|> do
    e <- parseExpression
    schar ';'
    return (SExpr e)

parseStatements :: Parser Statements
parseStatements = many1 parseStatement

getParse :: String -> Either ParseError Statements
getParse = parse parseStatements "sim"

run input = case getParse input of
  Right val -> "Found value: " ++ show val
  Left err -> "Parse error: " ++ show err

printParse = putStrLn . run