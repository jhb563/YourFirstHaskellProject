module Board where

import Data.Array
import Types

initializeBoard :: Int -> Int -> Board
initializeBoard height width = Board { boardArray = array arrayBounds assocList}
  where
    lowerBound = (1,1)
    upperBound = (height, width)
    arrayBounds = (lowerBound, upperBound)
    allIndicies = range arrayBounds
    assocList = map (\i -> (i, BlankCell)) allIndicies

elementAtIndex :: (Int, Int) -> Board -> Maybe BoardCell
elementAtIndex cell (Board arr) = if inRange (bounds arr) cell
  then Just $ arr ! cell
  else Nothing

updateBoardAtIndex :: (Int, Int) -> BoardCell -> Board -> Either InputError Board
updateBoardAtIndex cellIndex cellType (Board arr) = if not (inRange (bounds arr) cellIndex)
  then Left OutOfBoundsError
  else if arr ! cellIndex /= BlankCell
    then Left CellIsOccupiedError
    else Right (Board (arr // [(cellIndex, cellType)]))
