{- Parses ARFF Pairs. -}
module ARFFParser.Pairs (
  parsePairs
) where

import ARFFParser.AST
import ARFFParser.BasicCombinators
import ARFFParser.Junk
import ARFFParser.Value (parseIntLit, parseValue)

parsePairs :: Parser Char [Pair]
parsePairs
  = bracket
      (punctuation '{')
      (sepby' parsePair (punctuation ','))
      (punctuation '}')

parsePair :: Parser Char Pair
parsePair
  = Pair <$> parseIntLit <*> parseValue
