module Main where

import Split.LibSplit

main :: IO ()
main = do
  input <- getLine
  print (ourSplit input)
