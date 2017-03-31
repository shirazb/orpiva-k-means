module Main where

import KMeans.KMeans
import ARFFParser.File

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))
import System.IO (readFile)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (error "Usage: stack exec orpiva-k-means <path-to-arff-file>")
  file <- readFile (head args)
  let ast = parseARFFFile file
  case ast of
    Right (Just ((a, _), _)) -> print a
    Left err                 -> print err >> exitWith (ExitFailure 1)
    _                        -> error "Unsuccessful parse but parser did not fail."

  return ()
