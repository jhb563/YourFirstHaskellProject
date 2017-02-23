module Types where

import Data.Array
import Data.Function
import Data.List

data Player =
  FirstPlayer |
  SecondPlayer
  deriving (Eq)

instance Show Player where
  show FirstPlayer = "Player 1"
  show SecondPlayer = "Player 2"

data PlayerType =
  HumanPlayer |
  ComputerPlayer
  deriving (Eq)

instance Show PlayerType where
  show HumanPlayer = "(Human)"
  show ComputerPlayer = "(Computer)"

data BoardCell =
  BlankCell |
  FirstPlayerCell |
  SecondPlayerCell
  deriving (Eq)

instance Show BoardCell where
  show BlankCell = "   "
  show FirstPlayerCell = " X "
  show SecondPlayerCell = " O "

newtype Board = Board { boardArray :: (Array (Int, Int) BoardCell) }
  deriving (Eq)

instance Show Board where
  show (Board b) = intercalate interRowString allRowStrings
    where
      rowNumFromAssoc = fst . fst
      compareRowNum = (==) `on` rowNumFromAssoc
      groupedAssocs = groupBy compareRowNum (assocs b)
      discardIndex = map snd
      rows = map discardIndex groupedAssocs
      rowLength = length $ head rows
      rowString row = intercalate "|" (map show row) ++ "\n"
      allRowStrings = map rowString rows
      dashList = replicate rowLength " - "
      interRowString = intercalate " " dashList ++ "\n"

data InputError =
  OutOfBoundsError |
  CellIsOccupiedError
  deriving (Eq)

instance Show InputError where
  show OutOfBoundsError = "Sorry, that move is out of bounds!"
  show CellIsOccupiedError = "Sorry, that cell is already occupied!"

data GameResult =
  FirstPlayerWin |
  SecondPlayerWin |
  Draw
  deriving (Eq)

instance Show GameResult where
  show FirstPlayerWin = "Player 1 Wins!"
  show SecondPlayerWin = "Player 2 Wins!"
  show Draw = "It's a Draw!"

data Game = Game
  { gameBoard :: Board
  , currentPlayer :: Player
  , player1Type :: PlayerType
  , player2Type :: PlayerType }
  deriving (Eq)

instance Show Game where
  show game = unlines $
    [ show (gameBoard game)
    , show currentP ]
    where
      currentP = show (currentPlayer game) ++ " " ++ show currentPlayerMode
      currentPlayerMode = if currentPlayer game == FirstPlayer
        then player1Type game
        else player2Type game
