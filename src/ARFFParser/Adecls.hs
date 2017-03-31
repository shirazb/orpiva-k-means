{- Parser of ARFF adecls, data and data types. -}
module ARFFParser.Adecls (
  parseAdecls,
) where

import ARFFParser.AST
import ARFFParser.BasicCombinators
import ARFFParser.Junk
import ARFFParser.Value (parseIntLit, parseStringLit, parseValueList)
import ARFFParser.Pairs (parsePairs)

import Control.Applicative ((<|>), some, liftA2)

parseAdecls :: Parser Char [Adecl]
parseAdecls
  = some parseAdecl

parseAdecl :: Parser Char Adecl
parseAdecl
  = declaration "attribute" >> liftA2 Adecl parseStringLit parseDataType

parseData :: Parser Char Data
parseData
  = declaration "data" >> (pairData <|> valueData)
  where
    pairData  = PairData  <$> (concat <$> some parsePairs)
    valueData = ValueData <$> (concat <$> some parseValueList)

parseDataType :: Parser Char DataType
parseDataType
  =   (keyword "numeric" >> return Numeric)
  <|> (keyword "integer" >> return Integer)
  <|> (keyword "real"    >> return Real)
  <|> (keyword "string"  >> return String)
  <|> parseRelationalDataType
  <|> parseDateDataType
  <|> parseValueDataType

parseRelationalDataType :: Parser Char DataType
parseRelationalDataType = do
  keyword "relational"
  adecls <- parseAdecls
  declaration "end"
  s      <- parseStringLit
  return $ Relational adecls s

parseDateDataType :: Parser Char DataType
parseDateDataType = do
  keyword "date"
  str <- parseStringLit
  let str' = if str == "" then Nothing else Just str
  return $ DateDataT str'

parseValueDataType :: Parser Char DataType
parseValueDataType
  = ValueDataT <$>
      bracket
        (punctuation '}')
        parseValueList
        (punctuation '}')
