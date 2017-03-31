{- Top-level parser for whole ARFF files. -}
module ARFFParser.File (
  parseARFFFile
) where

import ARFFParser.Adecls (parseAdecls, parseData)
import ARFFParser.AST
import ARFFParser.BasicCombinators
import ARFFParser.Junk
import ARFFParser.Header

import Control.Applicative (liftA2)
import Control.Monad.State (get, unless)

parseARFFFile :: String -> Either Error (Maybe ((ARFF, String), Position))
parseARFFFile
  = runParser parseFile

parseFile :: Parser Char ARFF
parseFile
  = liftA2 ARFF parseHeader parseData <* endingParse

-- Ensures there is nothing left to parse after the end of the ARFF file.
endingParse :: Parser Char ()
endingParse = do
  junk
  leftover <- get
  unless (null leftover) (require failParser "Unexpected symbol")
