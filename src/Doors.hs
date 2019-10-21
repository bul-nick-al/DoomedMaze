module Doors where

import CodeWorld
import qualified Data.Array as A
import Maps
--data Door = Door (Int, Int) Int
--    deriving (Show, Eq)

--getClosedDoorColors :: [Door] ->  [Int]
--getClosedDoorColors doors  = map getCol doors
--  where
--    getCol (Door _ col) = col

getNewDoorsColors :: Vector -> Map -> [Color] -> [Color]
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


isInsideButton :: Vector -> Map -> Bool
isInsideButton (x, y) m = isButton candidate1 && isButton candidate2
            where
                candidate1 = m A.! (floor x, floor y)
                candidate2 = m A.! (ceiling (x - 0.99), ceiling (y - 0.99))
                isButton (Button _) = True
                isButton _ = False