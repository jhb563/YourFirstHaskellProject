module GameLogic where

import Control.Monad.State
import Text.Read hiding (lift, get)

import Board
import Types

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
