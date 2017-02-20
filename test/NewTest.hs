module Main where

import Split.LibSplit
import Test.Tasty.HUnit

main :: IO ()
main = assertEqual "Splitting Backpack: " ["B", "CKP", "CK"] (ourSplit "Backpack")
