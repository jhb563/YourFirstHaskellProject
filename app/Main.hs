module Main where

import Input

main :: IO ()
main = do
  pType <- getPlayer
  print pType
  dimens <- getDimensions
  print dimens
  move <- getMove
  print move
