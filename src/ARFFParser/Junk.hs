{- This module defines a number of combinators to handle lexical issues, such as
   removing whitespace and comments. It also handles distinguishing between
   keywords and identifiers.

   DISCLAIMER: I had previously written most of this code as part of a
               university compilers project.
-}

module ARFFParser.Junk where

import Control.Applicative (many, some, (<|>))
import Control.Monad       (void)
import Data.Char           (isSpace)
import Data.Maybe          (fromJust)

{- LOCAL IMPORTS -}
import ARFFParser.BasicCombinators

-- POST: Returns the symbol for a comment delimiter
commentDelim :: String
commentDelim
  = "%"

-- POST: Removes single line comments
comments :: Parser Char ()
comments = do
  string commentDelim
  many (satisfy (/= '\n'))
  require (char '\n') "Comment not terminated with newline."
  return ()

-- Post: Removes spaces incl \t,\n etc
spaces :: Parser Char ()
spaces
  = void $ some (satisfy isSpace)

-- POST: Removes whitespace or comments
junk :: Parser Char ()
junk
  = void $ many (spaces <|> comments)

-- POST: Removes leading whitespace
leadingWS :: Parser Char b -> Parser Char b
leadingWS p
  = junk >> p

-- POST: Removes trailing whitespace
trailingWS :: Parser Char b -> Parser Char b
trailingWS p = do
  parsedValue <- p
  junk
  return parsedValue

-- POST: Removes both leading and trailing whitespce
trimWS :: Parser Char b -> Parser Char b
trimWS
  = trailingWS . leadingWS

-- POST: Removes whitespace before and after the result of the string parser
token :: String -> Parser Char String
token
  = trimWS . string

-- POST: Parses ARFF declarations
declaration :: String -> Parser Char String
declaration s = trimWS $ do
  char '@'
  caselessString s

-- POST: Parses keywords defined in the WACC language
keyword :: String -> Parser Char String
keyword k = do
  kword <- leadingWS (caselessString k)
  check isSpace <|> check isPunctuation <|> check isComment
  junk
  return kword

-- POST: Returns True if the given input is either a seperator or an operator
isPunctuation :: Char -> Bool
isPunctuation
  =  flip elem ([';', ',', ']', '[', ')', '('] ++ operators)

-- POST: Returns True if you are at the start of a comment
isComment :: Char -> Bool
isComment
  = (==) '#'

-- POST: Parser for the given input char, it also removes whitespace around
--       the char
punctuation :: Char -> Parser Char Char
punctuation
  = trimWS . char

-- PRE:  The given input string contains a value which is present in the map
-- POST: It takes as input a map from strings to values of type a. It attempts
--       to parse one of the strings in the map and if it succeeds it will
--       return the corresponding a value. Essentially a parser lookup
--       function. It removes trailing WS
parseFromMap :: (Show a) => [(String, a)] -> Parser Char a
parseFromMap assoclist = do
  value  <- foldr1 (<|>) (map (token . fst) assoclist)
  return $ fromJust (lookup value assoclist)

-- POST: Similar to bracketNoWS defined in basic combinators, however it takes
--       whitespaces into account
bracket :: Parser Char a -> Parser Char b -> Parser Char c  -> Parser Char b
bracket open p close
  = trimWS $ bracketNoWS open p close

-- TODO: Define keywords for ARFF
-- POST: Returns the List of keywords defined in the wacc language
keywords :: [String]
keywords = []

-- TODO: Define operators for ARFF
operators = ['@']
