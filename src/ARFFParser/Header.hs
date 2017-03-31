{- Parses the ARFF Header. -}
module ARFFParser.Header (
  parseHeader
) where

import ARFFParser.Adecls (parseAdecls)
import ARFFParser.AST
import ARFFParser.BasicCombinators
import ARFFParser.Junk
import ARFFParser.Value (parseStringLit)

import Control.Applicative (liftA2)

parseHeader :: Parser Char Header
parseHeader
  = declaration "relation" >> liftA2 Header parseStringLit parseAdecls
