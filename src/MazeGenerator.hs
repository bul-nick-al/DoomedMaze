{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MazeGenerator where

import Data.Monoid
import System.Random

import Space

-- ==============================================
-- Модель
-- ==============================================

-- | Ячейка стены лабиринта.
type Brick = Coords

-- | Лабиринт — это пространство с ячейками стен.
type Maze = Space String -- !



-- | Внешняя граница лабиринта.
borderMaze :: Area -> Maze
borderMaze area = fromCoordsList ( concat
    [ map (\y -> (l - 1, y)) [b-1..t+1]
    , map (\y -> (r + 1, y)) [b-1..t+1]
    , map (\x -> (x, b - 1)) [l-1..r+1]
    , map (\x -> (x, t + 1)) [l-1..r+1]
    ] )
    where
        Area (l, b) (r, t) = area

-- | Добавить лабиринту внешнюю границу.
withBorder :: Maze -> Maze
withBorder maze = maze <> foldMap borderMaze (spaceArea maze)

-- ==============================================
-- Генерация лабиринта
-- ==============================================

-- | Транспонировать лабиринт.
transposeMaze :: Maze -> Maze
transposeMaze = fmap swap . transposeSpace

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
mkVerticalWall :: Coords -> Area -> Maze
mkVerticalWall (i, j) (Area (_, b) (_, t)) = fromCoordsList
    [ (i, y) | y <- [b..t], y /= j ]

-- | Сгенерировать случайный лабиринт в заданной прямоугольной области.
-- Генерация происходит при помощи рекурсивного разделения области
-- стенами с одним проходом.
genMaze :: RandomGen g => Area -> g -> Maze
genMaze area g
    | thinArea area = mempty { spaceArea = Just area }
    | otherwise     = wall <> leftMaze <> rightMaze
    where
    ((i, j), g')  = randomWallCoords area g
    wall          = mkVerticalWall (i, j) area
    (left, right) = splitH i area

    -- для левой и правой подобластей мы вызываем genMaze рекурсивно,
    -- транспонируя обе подобласти, а затем транспонируя обратно
    -- полученные подлабиринты
    (gl, gr) = split g'
    leftMaze  = transposeMaze (genMaze (transposeArea left)  gl)
    rightMaze = transposeMaze (genMaze (transposeArea right) gr)



maze:: RandomGen g => g -> Maze
maze g = genMaze (Area (0,0) (16,29)) g
----------------------------------------------------------------
mazeToGrid :: [(Int,Int)] -> Maze -> [[String]] -> [[String]]
mazeToGrid [] _ res = res
mazeToGrid (x:xs) m res
    | isWall x m =  mazeToGrid xs m (res ++ [["|"]])
    | otherwise =  mazeToGrid xs m (res ++ [["."]]) 
    -- | isWall x m = (res ++ [["|"]]) ++ mazeToGrid xs m res
    -- | otherwise = (res ++ [["."]]) ++ mazeToGrid xs m
    -- -- where 
    --     m = Space{spaceArea = spaceArea, spaceObjects = spaceObjects}

getAreaFromMaze :: Maze -> Area
getAreaFromMaze Space {..} = case spaceArea of
                                Just value -> value
                                Nothing    ->  Area (0,0) (0,0) 
    -- | spaceArea == Nothing = Area (0,0) (0,0)
    -- | spaceArea == Just (Area) = Area a b    

isWall:: Coords -> Maze -> Bool
isWall coords Space{..} = any (\x -> (fst x) == coords) spaceObjects 

-- generateGrid :: Int -> Int -> [[String]]
generateGrid width height = replicate height (replicate width "|") 
