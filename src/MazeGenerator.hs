{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MazeGenerator where

import Data.Monoid
import System.Random
import Debug.Trace
import Data.List

import Space

type Maze = Space String

-- | Utility function for debugging 
prnt ::Show a => a -> a
prnt s = trace (show s) s

-- | Transpose maze
transposeMaze :: Maze -> Maze
transposeMaze = transposeSpace 

-- | Generating random coords for vertical wall. 
-- X coordinate shows the position of wall, coordinate y - the position of hole in the wall
-- In order to prevent changes gluing and keep maze connected, walls are created on odd 
-- positions, ways on even.
randomWallCoords :: RandomGen g => Area -> g -> (Coords, g)
randomWallCoords (Area (l, b) (r, t)) g = ((2 * i + 1, 2 * j), g'')
    where
    (i, g')  = randomR (l `div` 2, (r - 1) `div` 2) g
    (j, g'') = randomR ((b + 1) `div` 2, t `div` 2) g'

-- | Create a Maze object for the wall, given coords, Area and object on the pass
createVerticalWall :: Coords -> Area -> String -> Maze
createVerticalWall (i, j) (Area (_, b) (_, t)) obj = fromList coordsList
    where 
        coordsList = zip  [ (i, y) | y <- range ] ks
        ks = map getK range
        range = [b..t]
        getK y' = if y' == j then obj else "|"


allDoors :: [String]
allDoors = ["a", "b", "c", "d", "e"]

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

-- | Function to generate button in random position.
generateButtonRandomly :: RandomGen g => g -> Area -> String -> (Maze,g)
generateButtonRandomly g (Area (l, b) (r, t)) obj = (maze, g'')
    where 
        (i, g')  = randomR (l,r) g
        (j, g'') = randomR (b,t) g'
        maze = prnt(singleton ( i,j) obj)


deleteObjFromList :: [String] -> String -> [String]
deleteObjFromList [] _ = []
deleteObjFromList lst str = delete str lst

-- | Function that guarantees that exit is available.
clearExit:: Maze -> Maze
clearExit m = newMaze
    where
        Area (s, t) (a, b) = getAreaFromMaze m
        updatedSpaceObjects = delete ((a,b),"|") (getObjsFromMaze m)
        updatedSpaceObjects' = delete ((a-1,b),"|")  (getObjsFromMaze m)
        updatedSpaceObjects'' =delete ((a,b-1),"|")  (getObjsFromMaze m)
        newMaze = Space {spaceArea =Just (Area (s, t) (a, b)), spaceObjects = updatedSpaceObjects''}

-- | Function to delete unneeded objects from Maze
filterEmpty:: Maze -> Maze
filterEmpty m = newMaze
    where
        updatedSpaceObjects = filter isEmpty (getObjsFromMaze m)
        isEmpty (a,b) = if b =="." then False else True
        newMaze = Space {spaceArea =Just (getAreaFromMaze m), spaceObjects = updatedSpaceObjects}


-- | Function to recursively generate maze. Every time maze is divided into 2 and each part is generated recursively
genMaze :: RandomGen g => Area -> g -> [String] -> [String] -> Maze
genMaze area g unOpened opened
    | thinArea area = newButton<>(mempty { spaceArea = Just area })  
    | otherwise     =  newButton <> wall <> leftMaze <> rightMaze
    where
        button = case head' opened of --
            Nothing -> "."
            Just a -> a 
        (newButton, g'o)  = if button == "." then (empty, g) else generateButtonRandomly g area button
        newOpened = deleteObjFromList opened button

        door = case head' unOpened of --  
                Nothing -> "."
                Just a -> a
        updatedOpened = if door == "." then newOpened else newOpened ++ [toUpper door]
        newUs = deleteObjFromList unOpened door

        ((i, j), g')  = randomWallCoords area g'o
        wall  = createVerticalWall (i,j) area door
        (left, right) = splitH i area
        (gl, gr) = split g'
        leftMaze  = transposeMaze (genMaze (transposeArea left)  gl newUs updatedOpened)
        rightMaze = transposeMaze (genMaze (transposeArea right) gr newUs updatedOpened)

-- | Custom function to prevent "Space" naming conflicts 
toUpper :: String -> String
toUpper s
    | s == "a" = "A"
    | s == "b" = "B"
    | s == "c" = "C"
    | s == "d" = "D"
    | s == "e" = "E"
    | s == "A" = "A"
    | s == "B" = "B"
    | s == "C" = "C"
    | s == "D" = "D"
    | s == "E" = "E"

-- | Generation of exit coords. 
exit :: Coords -> Maze
exit cors = singleton cors "$"

-- | Function to generate random coords in range
generateRandomCoords :: RandomGen g => g -> Area -> (Coords, g)
generateRandomCoords g (Area (l, b) (r, t)) = ((i,j), g'')
    where 
        (i, g')  = randomR (l,r) g
        (j, g'') = randomR (b,t) g'

-- | Function for recursive generation of batteries.
generateBatteriesList :: RandomGen g => g-> Maze -> [(Coords, String)] -> Int -> [(Coords, String)]
generateBatteriesList gen m res lim
    | length res >= lim = res
    | isObject ranCoords m = generateBatteriesList gen' m res lim
    | otherwise = generateBatteriesList gen' m ([(ranCoords,"#")] ++ res) lim
        where
            (ranCoords, gen') = generateRandomCoords gen (getAreaFromMaze m)

-- | Function to generate batteries for appending
addBatteries :: RandomGen g => g -> Maze -> Int -> Maze
addBatteries g m lim = updatedMaze
    where
        updatedMaze = Space {spaceArea = Just (getAreaFromMaze m), spaceObjects = oldMazeObjects ++ updatedSpaceObjects}
        oldMazeObjects = getObjsFromMaze m
        updatedSpaceObjects = generateBatteriesList g m [] lim

-- | Generate level with given difficulty
generateDifficultLevel ::RandomGen g => g -> Int  -> Maze
generateDifficultLevel g dif
    | dif < 3 = mazeModification (genMaze (Area (0,0) (10+dif, 11+dif)) g (doorsList 1) [] ) 1
    | dif < 5 = mazeModification(genMaze (Area (0,0) (13+dif, 15+dif)) g (doorsList 2) [] ) 1 
    | dif < 7 = mazeModification(genMaze (Area (0,0) (15+dif, 21+dif)) g (doorsList 3) []) 2
    | dif < 9 = mazeModification (genMaze (Area (0,0) (20+dif, 25+dif)) g (doorsList 4) [] ) 3
    | otherwise = mazeModification (genMaze (Area (0,0) (10 + dif *2, 15 + dif*2)) g (doorsList 5) []) (dif `div` 2)
        where
            doorsList num = take num allDoors
            mazeModification maze bs = addBatteries g (clearExit (filterEmpty (maze <> appendExit maze))) bs
            appendExit m = exit (getMazeSize m)

-- | Function to convert maze to array
mazeToGrid :: [(Int,Int)] -> Maze -> [[String]] -> [[String]]
mazeToGrid [] _ res = res
mazeToGrid (x:xs) m res
    | isNotFloor =  mazeToGrid xs m (res ++ [[object]])
    | otherwise =  mazeToGrid xs m (res ++ [["."]])
        where
            object = getObjectFromCoords x spaceObjects
            isNotFloor = isObject x m
            Space{..} = m

-- | Getter of object given coords. 
getObjectFromCoords :: (Int,Int) -> [(Coords, String)] -> String
getObjectFromCoords _ [] = "."
getObjectFromCoords coords ((c, o):xs)
    | coords == c = o
    | otherwise = getObjectFromCoords coords xs

-- | Getter of area given maze.
getAreaFromMaze :: Maze -> Area
getAreaFromMaze Space {..} = case spaceArea of
                                Just value -> value
                                Nothing    ->  Area (0,0) (0,0) 

-- | Get maze size
getMazeSize :: Maze -> Coords
getMazeSize m = getSecondCoords area
    where 
        area = getAreaFromMaze m 
        getSecondCoords (Area c d) = d 

-- | Getter of objects from maze.
getObjsFromMaze :: Maze -> [(Coords,String)]
getObjsFromMaze Space {..} = spaceObjects

-- | Check if there is an object on the coords.  
isObject:: Coords -> Maze -> Bool
isObject coords Space{..} = any (\x -> (fst x) == coords) spaceObjects 
