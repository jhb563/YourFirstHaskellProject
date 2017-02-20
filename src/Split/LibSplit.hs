module Split.LibSplit where

import Data.Char (toUpper)
import Data.List.Split (splitOn)

ourSplit :: String -> [String]
ourSplit input = splitOn "A" (map toUpper input)
