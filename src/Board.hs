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

updateBoardAtIndex :: (Int, Int) -> BoardCell -> Board -> Maybe Board
updateBoardAtIndex cellIndex cellType (Board arr) = if inRange (bounds arr) cellIndex
  then Just (Board (arr // [(cellIndex, cellType)]))
  else Nothing
