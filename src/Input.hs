module Input where

import Data.Char
import Text.Read
import Data.String.Utils

import Types

getPlayer :: IO PlayerType
getPlayer = do
  putStrLn "Who do you want to play against? (human/computer)"
  processPlayerInput
  where
    processPlayerInput = do
      input <- getLine
      let sanitizedInput = map toLower (strip input)
      case sanitizedInput of
        "human" -> return HumanPlayer
        "computer" -> return ComputerPlayer
        _ -> do
          putStrLn "Sorry, that isn't a valid mode. Enter \"human\" or \"computer\"."
          processPlayerInput

getDimensions :: IO (Int, Int)
getDimensions = do
  putStrLn "How many rows should the board have? (at least 3)"
  rows <- processIntInput
  putStrLn "How many columns should the board have? (at least 3)"
  cols <- processIntInput
  return (rows, cols)
  where
    processIntInput = do
      input <- getLine
      case readMaybe input of
        Just i -> if i >= 3
          then return i
          else printFailure
        Nothing -> printFailure
    printFailure = do
      putStrLn "Please enter a whole number bigger than 2."
      processIntInput

getMove :: IO (Int, Int)
getMove = do
  putStrLn "Please enter a move (ex. \"1 1\")."
  processMoveInput
  where
    processMoveInput = do
      input <- getLine
      case words input of
        [row, col] -> case (readMaybe row, readMaybe col) of
          (Just r, Just c) -> return (r,c)
          _ -> printFailure
        _ -> printFailure
    printFailure = do
      putStrLn "Please enter two numbers separated by a space."
      processMoveInput
