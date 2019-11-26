{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MazeGenerator where

import Data.Monoid
import System.Random
import Debug.Trace
import Data.List

import Space

type Maze = Space String

prnt ::Show a => a -> a
prnt s = trace (show s) s

-- | Транспонировать лабиринт.
transposeMaze :: Maze -> Maze
transposeMaze = transposeSpace --fmap swap . transposeSpace 

-- | Сгенерировать случайные координаты для вертикальной стены.
-- Координата x указывает положение стены, а координата y — положение прохода в стене.
-- Чтобы стены не склеивались и лабиринт оставался связным,
-- стены всегда создаются на нечётных позициях, а проходы — на чётных.
randomWallCoords :: RandomGen g => Area -> g -> (Coords, g)
randomWallCoords (Area (l, b) (r, t)) g = ((2 * i + 1, 2 * j), g'')
    where
    (i, g')  = randomR (l `div` 2, (r - 1) `div` 2) g
    (j, g'') = randomR ((b + 1) `div` 2, t `div` 2) g'

-- | Создать вертикальную стену с заданными координатами.
-- Координата x указывает положение стены, а координата y — положение прохода в стене.
-- mkVerticalWall :: Coords -> Area -> Maze
-- mkVerticalWall (i, j) (Area (_, b) (_, t)) = fromCoordsList [ (i, y) | y <- [b..t], y /= j ]

-- | Сгенерировать случайный лабиринт в заданной прямоугольной области.
-- Генерация происходит при помощи рекурсивного разделения области
-- стенами с одним проходом.
--
-- для левой и правой подобластей мы вызываем genMaze рекурсивно,
-- транспонируя обе подобласти, а затем транспонируя обратно
-- полученные подлабиринты


wallCoordsToMazeWithObj :: String -> Coords -> Area -> Maze
wallCoordsToMazeWithObj obj (i, j) (Area (_, b) (_, t)) = fromList coordsList
    where 
        coordsList = zip  [ (i, y) | y <- range ] ks
        ks = map getK range
        range = [b..t]
        getK y' = if y' == j then "|" else obj

createRandomWall :: RandomGen g => Area -> g -> String -> (Maze, g, Int) -- !
createRandomWall (Area (l, b) (r, t)) g obj = (wall, g'', i)
    where
        area = Area (l, b) (r, t)
        (i, g')  = randomR (l `div` 2, (r - 1) `div` 2) g
        (j, g'') = randomR ((b + 1) `div` 2, t `div` 2) g'
        wall = wallCoordsToMazeWithObj obj (2 * i + 1, 2 * j) area


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


generateButtonRandomly :: RandomGen g => g -> Area -> String -> (Maze,g)
generateButtonRandomly g (Area (l, b) (r, t)) obj = (maze, g'')
    where 
        (i, g')  = randomR (l,r) g
        (j, g'') = randomR (b,t) g'
        maze = prnt(singleton ( i,j) obj)


deleteObjFromList :: [String] -> String -> [String]
deleteObjFromList [] _ = []
deleteObjFromList lst str = delete str lst

clearExit:: Maze -> Maze
clearExit m = newMaze
    where
        Area (s, t) (a, b) = getAreaFromMaze m
        updatedSpaceObjects = delete ((a,b),"|") (getObjsFromMaze m)
        updatedSpaceObjects' = delete ((a-1,b),"|")  (getObjsFromMaze m)
        updatedSpaceObjects'' = delete ((a,b-1),"|")  (getObjsFromMaze m)
        newMaze = Space {spaceArea =Just (Area (s, t) (a, b)), spaceObjects = updatedSpaceObjects''}

filterEmpty:: Maze -> Maze
filterEmpty m = newMaze
    where
        updatedSpaceObjects = filter isEmpty (getObjsFromMaze m)
        isEmpty (a,b) = if b =="." then False else True
        newMaze = Space {spaceArea =Just (getAreaFromMaze m), spaceObjects = updatedSpaceObjects}

-- splitlist :: [a] -> ([a], [a])
-- splitlist [] = ([],[]) 
-- splitlist xs = splitAt ((length xs + 1) `div` 2) xs

-- sortAndSplit :: Ord a => [a] -> ([a],[ a])
-- sortAndSplit xs = splitlist (sort xs) 


--DoorsNum, needClose, Available
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
        leftMaze  = transposeMaze (genMaze (transposeArea left)  gl newUs updatedOpened) -- s3 s4)--newUs newOS)
        rightMaze = transposeMaze (genMaze (transposeArea right) gr newUs updatedOpened)


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

exit :: Coords -> Maze
exit cors = singleton cors "$"

generateDifficultLevel ::RandomGen g => g -> Int  -> Maze
generateDifficultLevel g dif
    | dif < 3 = filterEmpty (clearExit (genMaze (Area (0,0) (10+dif, 11+dif)) g (doorsList 1) [] <> exit (10+dif, 11+dif)))
    | dif < 5 = filterEmpty (clearExit (genMaze (Area (0,0) (13+dif, 15+dif)) g (doorsList 2) [] <> exit (13+dif, 15+dif)))
    | dif < 7 = filterEmpty (clearExit (genMaze (Area (0,0) (15+dif, 21+dif)) g (doorsList 3) [] <> exit (15+dif, 21+dif)))
    | dif < 9 = filterEmpty (clearExit (genMaze (Area (0,0) (20+dif, 25+dif)) g (doorsList 4) [] <> exit (20+dif, 25+dif)))
    | otherwise = filterEmpty (clearExit (genMaze (Area (0,0) (10 + dif *2, 15 + dif*2)) g (doorsList 5) [] <> exit (10 + dif *2, 15 + dif*2)))
        where
            doorsList num = take num allDoors


mazeToGrid :: [(Int,Int)] -> Maze -> [[String]] -> [[String]]
mazeToGrid [] _ res = res
mazeToGrid (x:xs) m res
    | isNotFloor =  mazeToGrid xs m (res ++ [[object]])
    | otherwise =  mazeToGrid xs m (res ++ [["."]])
        where
            object = getObjectFromCoords x spaceObjects
            isNotFloor = isObject x m
            Space{..} = m

getObjectFromCoords :: (Int,Int) -> [(Coords, String)] -> String
getObjectFromCoords _ [] = "."
getObjectFromCoords coords ((c, o):xs)
    | coords == c = o
    | otherwise = getObjectFromCoords coords xs

getAreaFromMaze :: Maze -> Area
getAreaFromMaze Space {..} = case spaceArea of
                                Just value -> value
                                Nothing    ->  Area (0,0) (0,0) 

getObjsFromMaze :: Maze -> [(Coords,String)]
getObjsFromMaze Space {..} = spaceObjects

isObject:: Coords -> Maze -> Bool
isObject coords Space{..} = any (\x -> (fst x) == coords) spaceObjects 

-- generateGrid width height = replicate height (replicate width "|") 
