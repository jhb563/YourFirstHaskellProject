module Main where

import GameLogic
import Input

main :: IO ()
main = do
  pType <- getPlayer
  dimens <- getDimensions
  res <- runGame dimens pType
  print res
