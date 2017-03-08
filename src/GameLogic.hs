module GameLogic where

import Control.Monad.State
import Data.Array
import System.Random
import Text.Read hiding (lift, get)

import Board
import Types

-- State functions for playing the current move

currentCell :: StateT Game IO BoardCell
currentCell = do
  player <- gets currentPlayer
  case player of
    FirstPlayer -> return FirstPlayerCell
    SecondPlayer -> return SecondPlayerCell

playHumanMove :: StateT Game IO (Int, Int)
playHumanMove = do
  lift $ putStrLn "Please enter a move (ex. \"1 1\")."
  processMoveInput
  where
    processMoveInput = do
      input <- lift getLine
      case words input of
        [row, col] -> case (readMaybe row, readMaybe col) of
          (Just r, Just c) -> do
            currentGame <- get
            cell <- currentCell
            case updateBoardAtIndex (r, c) cell (gameBoard currentGame) of
              Left err -> do
                lift $ print err
                processMoveInput
              Right newBoard -> do
                put (currentGame { gameBoard = newBoard })
                return (r,c)
          _ -> printFailure
        _ -> printFailure
    printFailure = do
      lift $ putStrLn "Please enter two numbers separated by a space."
      processMoveInput

playComputerMove :: StateT Game IO (Int, Int)
playComputerMove = do
  currentGame <- get
  let board@(Board arr) = gameBoard currentGame
  cell <- currentCell
  let choices = [i | i <- indices arr, arr ! i == BlankCell]
  indexOfChoice <- lift $ getStdRandom (randomR (0, (length choices) - 1))
  let finalChoice = choices !! indexOfChoice
  case updateBoardAtIndex finalChoice cell board of
    Left err -> do
      lift $ putStrLn "The Computer picked an illegal move..."
      lift $ print err
      playComputerMove
    Right newBoard -> do
      put (currentGame { gameBoard = newBoard })
      return finalChoice
