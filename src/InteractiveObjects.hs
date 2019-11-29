module InteractiveObjects where

import CodeWorld
import qualified Data.Array as A
import Maps

-- | Function to get updated list of opened doors
getNewDoorsColors
  :: (Int, Int) -- Current position
  -> Map -- World map
  -> [Color] -- Current list of doors
  -> [Color] -- New list of doors
getNewDoorsColors (x, y) m colors
    = case m A.! (x, y) of
      (Button color) -> if color `elem` colors
                        then filter (\e -> e/=color) colors
                        else color : colors
      _ ->  colors


-- | Function to check if current player position is inside button
isInsideButton :: Vector -> Map -> (Bool, Maybe (Int, Int))
isInsideButton (x, y) m = isInside (x, y) m isButton
            where
                isButton (Button _) = True
                isButton _ = False

-- | Function to check if current player position is inside button
isInsideBattery :: Vector -> Map -> (Bool, Maybe (Int, Int))
isInsideBattery (x, y) m = isInside (x, y) m isBattery
            where
                isBattery Battery = True
                isBattery _ = False



-- | Function to check if current player position is inside exit
isInsideExit :: Vector -> Map -> (Bool, Maybe (Int, Int))
isInsideExit (x, y) m = isInside (x, y) m isBattery
            where
                isBattery Exit = True
                isBattery _ = False

isInside
    :: Vector
    -> Map
    -> (GameObject -> Bool)
    -> (Bool, Maybe (Int, Int))
isInside (x, y) m isObject
    = getCandidate (isObject candidate1) (isObject candidate2)
            where
                candidate1 = m A.! candidatePos1
                candidate2 = m A.! candidatePos2
                candidatePos1 = (floor x, floor y)
                candidatePos2 = (ceiling (x - 0.99), ceiling (y - 0.99))
                getCandidate True _ = (True, Just candidatePos1)
                getCandidate _ True = (True, Just candidatePos2)
                getCandidate _ _ = (False, Nothing)
