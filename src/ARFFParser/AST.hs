{-
  The AST of an ARFF file.

  The context free grammar is taken from
      https://list.waikato.ac.nz/pipermail/wekalist/2008-January/038545.html
-}

module ARFFParser.AST where

data ARFF
  = ARFF Header Data
data Header
  = Header String [Adecl] -- cannot be empty

data Adecl
  = Adecl String DataType

data DataType
  = Numeric
  | Integer
  | Real
  | String
  | Relational [Adecl] String -- cannot be empty
  | DateData Date
  | Values [Value] -- cannot be empty

data Date
  = Date (Maybe String)

data Data
  = PairData [Pair] -- cannot be empty
  | ValueData [Value] -- cannot be empty

data Pair
  = Pair Int Value

-- Ensure only valid floats, ints, strings parsed
data Value
  = QMarkVal
  | FloatVal Float
  | IntVal Int
  | StringVal String
