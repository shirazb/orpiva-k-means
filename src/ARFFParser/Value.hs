{- Parser of ARFF Values. -}
module ARFFParser.Value (
  parseValue
) where

import ARFFParser.AST
import ARFFParser.BasicCombinators
import ARFFParser.Junk

import Control.Applicative ((<|>), some, many, (*>))

parseValue :: Parser Char Value
parseValue
  = parseQMark
  <|> parseFloat
  <|> parseInt
  <|> parseString

-- Parses a single signed integer Value.
parseInt :: Parser Char Value
parseInt
  = IntVal <$> parseIntLit

-- Parses a single signed float.
parseFloat :: Parser Char Value
parseFloat = do
  sign           <- parseSign
  digits         <- many digit
  char '.'
  decimals       <- some digit
  floatExp       <- parseExponent <|> return 1
  let floatBase  = sign (read (digits ++ "." ++ decimals))
  return $ FloatVal (floatBase ^ floatExp)

-- Parses a string value.
parseString :: Parser Char Value
parseString
  = StringVal <$> parseStringLit

-- Parses a question mark value.
parseQMark :: Parser Char Value
parseQMark
  = punctuation '?' *> return QMarkVal

-- Parses the exponent part of a float
parseExponent :: Parser Char Int
parseExponent
  = (punctuation 'E' <|> punctuation 'e') *> parseIntLit

-- Parses a plus or minus sign that indicates the sign of a number.
parseSign :: Num a => Parser Char (a -> a)
parseSign = do
  sign <- string "-" <|> string "+" <|> return ""
  return $ if sign == "-" then id else negate

-- Parses a single signed integer literal as an integer.
parseIntLit :: Parser Char Int
parseIntLit
  = parseSign <*> (read <$> some digit)

-- I really dont understand the EBNF for this, so I have resorted to
-- this definition...
parseStringLit :: Parser Char String
parseStringLit
  = bracket (token "\"") (many character) (token "\"")
