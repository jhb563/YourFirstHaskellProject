module GameLogic 
  ( pickComputerMove
  , playHumanMove
  , evaluateMove )
  where

import Control.Monad.State
import Data.Array
import Data.Function
import Data.List
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

evaluateMove :: 
  (Int, Int) -> -- The Move
  Board -> -- The current board
  Maybe GameResult -- New Board or Error, and Result
evaluateMove move board = case elementAtIndex move board of
  Nothing -> Nothing
  Just BlankCell -> Nothing
  Just cell -> result
    where
      winResult = if cell == FirstPlayerCell then FirstPlayerWin else SecondPlayerWin
      (_, (rows, cols)) = bounds $ boardArray board
      threshold = min rows cols
      verticalWin = verticalContinuityForMove move cell board >= threshold
      horizontalWin = horizontalContinuityForMove move cell board >= threshold
      diagonalULWin = diagonalContinuityForMoveUL move cell board >= threshold
      diagonalLRWin = diagonalContinuityForMoveLR move cell board >= threshold
      boardFull = length (filter (== BlankCell) (elems (boardArray board))) == 0
      result = if verticalWin || horizontalWin || diagonalULWin || diagonalLRWin
        then Just winResult
        else if boardFull
          then Just Draw
          else Nothing

verticalContinuityForMove :: (Int, Int) -> BoardCell -> Board -> Int
verticalContinuityForMove (_, col) = continuityHelper columnFilter
  where
    columnFilter = \((_, c), _) -> c == col

horizontalContinuityForMove :: (Int, Int) -> BoardCell -> Board -> Int
horizontalContinuityForMove (row, _) = continuityHelper rowFilter
  where
    rowFilter = \((r, _), _) -> r == row

diagonalContinuityForMoveUL :: (Int, Int) -> BoardCell -> Board -> Int
diagonalContinuityForMoveUL (row, col) = continuityHelper diagonalULFilter
  where
    diagonalULFilter = \((a,b), _) -> a - b == row - col

diagonalContinuityForMoveLR :: (Int, Int) -> BoardCell -> Board -> Int
diagonalContinuityForMoveLR (row, col) = continuityHelper diagonalLRFilter
  where
    diagonalLRFilter = \((a,b), _) -> a + b == row + col

continuityHelper :: (((Int, Int), BoardCell) -> Bool) -> BoardCell -> Board -> Int
continuityHelper filterFunc cell (Board arr) = foldl foldMax 0 groupings
  where
    cellsInRange = filter filterFunc (assocs arr)
    groupings = groupBy ((==) `on` snd) cellsInRange
    foldMax currentMax [] = currentMax
    foldMax currentMax l@((_, cellType) : _) = if cellType == cell
      then max currentMax (length l)
      else currentMax
