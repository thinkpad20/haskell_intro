module Parser where

import Text.ParserCombinators.Parsec
import AST

schar c = spaces >> char c >> spaces
sstring s = spaces >> string s >> spaces

parseInt :: Parser Int
parseInt = spaces >> (fmap read $ many1 digit)

parseId :: Parser String
parseId = spaces >> many1 letter

parseTrue, parseFalse :: Parser Term
parseTrue = spaces >> string "true" >> return (TBool True)
parseFalse = spaces >> string "false" >> return (TBool False)

parseParens :: Parser Expression
parseParens = do
  spaces >> char '(' >> spaces
  e <- parseExpression
  spaces >> char ')'
  return e

parseTerm :: Parser Term
parseTerm = fmap Num parseInt 
  <|> try parseTrue
  <|> try parseFalse
  <|> fmap Var parseId

parseNot :: Parser Expression
parseNot = spaces >> char '!' >> fmap Not parseExpression

parseExpression :: Parser Expression
parseExpression = getTerm <|> parseParens <|> parseNot
  where getTerm = do
          term <- parseTerm
          let t = ETerm term
          spaces
          sym <- optionMaybe (oneOf ['+', '-', '>', '<'])
          case sym of
            Nothing -> return t
            Just '+' -> fmap (Plus t) parseExpression
            Just '-' -> fmap (Minus t) parseExpression
            Just '<' -> fmap (Lt t) parseExpression
            Just '>' -> fmap (Gt t) parseExpression


parseAssign :: Parser Statement
parseAssign = spaces >> do
  var <- parseId
  e <- (schar '=' >> parseExpression)
  schar ';'
  return (Assign var e)

parseIf :: Parser Statement
parseIf = spaces >> do
  cond <- (sstring "if" >> parseExpression)
  ifTrue <- (sstring "then" >> parseExpression)
  ifFalse <- (sstring "else" >> parseExpression)
  schar ';'
  return (If cond ifTrue ifFalse)

parseStatement :: Parser Statement
parseStatement = try parseIf
  <|> try parseAssign
  <|> fmap SExpr parseExpression

parseStatements :: Parser Statements
parseStatements = many1 parseStatement

getParse :: String -> Either ParseError Statements
getParse = parse parseStatements "sim"

run input = case getParse input of
  Right val -> "Found value: " ++ show val
  Left err -> "Parse error: " ++ show err

printParse = putStrLn . run