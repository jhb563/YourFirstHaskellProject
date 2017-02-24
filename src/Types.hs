module Types where

import Data.Array

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
  show _ = undefined

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
  , movesPlayed :: Int
  , currentPlayer :: Player
  , player1Type :: PlayerType
  , player2Type :: PlayerType
  , gameResult :: Maybe GameResult }
  deriving (Eq)

instance Show Game where
  show game = unlines $
    [ show (gameBoard game)
    , show resultOrCurrentPlayer ]
    where
      resultOrCurrentPlayer = case gameResult game of
        Just result -> show result
        Nothing -> show (currentPlayer game) ++ show currentPlayerMode
      currentPlayerMode = if currentPlayer game == FirstPlayer
        then player1Type game
        else player2Type game
