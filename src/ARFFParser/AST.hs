{-
  The AST of an ARFF file.

  The context free grammar is taken from
      https://list.waikato.ac.nz/pipermail/wekalist/2008-January/038545.html
-}

module ARFFParser.AST where

data ARFF
  = ARFF Header Data
  deriving (Show, Eq)

data Header
  = Header String [Adecl]
  deriving (Show, Eq)

data Adecl
  = Adecl String DataType
  deriving (Show, Eq)

data DataType
  = Numeric
  | Integer
  | Real
  | String
  | Relational [Adecl] String
  | DateDataT (Maybe String)
  | ValueDataT [Value]
  deriving (Show, Eq)

data Data
  = PairData [Pair]
  | ValueData [Value]
  deriving (Show, Eq)

data Pair
  = Pair Int Value
  deriving (Show, Eq)

data Value
  = QMarkVal
  | FloatVal Float
  | IntVal Int
  | StringVal String
  deriving (Show, Eq)
