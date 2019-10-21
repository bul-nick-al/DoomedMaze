module Doors where

import CodeWorld
data Door = Door (Int, Int) Int
    deriving (Show, Eq)

getClosedDoorColors :: [Door] ->  [Int]
getClosedDoorColors doors  = map getCol doors
  where
    getCol (Door _ col) = col  

getNewDoors :: Vector -> [Door] -> [Door]
getNewDoors _ [] = []
getNewDoors (x,y) (d:ds)
    | getTriggerX d `elem` [round x, ceiling (x - 0.99)] && getTriggerY d `elem` [round y, ceiling (y - 0.99)] = (getNewDoors (x,y) ds)
    | otherwise = [d] ++ getNewDoors (x,y) ds 
    where
        getTriggerX (Door (x,_) _) = x 
        getTriggerY (Door (_,y) _) = y 