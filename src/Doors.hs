module Doors where

import CodeWorld
import qualified Data.Array as A
import Maps

-- | Function to get updated list of opened doors
getNewDoorsColors :: 
  Vector -- Current position
  -> Map -- World map
  -> [Color] -- Current list of doors
  -> [Color] -- New list of doors
getNewDoorsColors (x, y) m colors = case candidate1 of
                                    (Button color) -> if color `elem` colors
                                                      then filter (\e -> e/=color) colors
                                                      else color : colors
                                    _ ->  case candidate2 of
                                          (Button color) -> if color `elem` colors
                                                            then filter (\e -> e/=color) colors
                                                            else color : colors
                                          _ -> colors

            where
                candidate1 = m A.! (floor x, floor y)
                candidate2 = m A.! (ceiling (x - 0.99), ceiling (y - 0.99))

-- | Function to check if current player position is inside button
isInsideButton :: Vector -> Map -> Bool
isInsideButton (x, y) m = isButton candidate1 && isButton candidate2
            where
                candidate1 = m A.! (floor x, floor y)
                candidate2 = m A.! (ceiling (x - 0.99), ceiling (y - 0.99))
                isButton (Button _) = True
                isButton _ = False