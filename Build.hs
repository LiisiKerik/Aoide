--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Main (main) where
  import System.Process (callCommand)
  main :: IO ()
  main =
    do
      callCommand "cabal new-clean"
      callCommand "cabal check"
      callCommand "cabal new-build"
      callCommand "cabal new-haddock"
--------------------------------------------------------------------------------------------------------------------------------