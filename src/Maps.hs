module Maps where


import CodeWorld
import Vectors
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String
import MazeGenerator
import Space
import System.Random

data GameObject = Wall | Door Color | Button Color | Floor| Entrance | Exit deriving (Show, Eq)
type Map = A.Array (Int, Int) GameObject


-- | Level data type
data Level = Level {
        levelMap :: [String]
      , openColors :: [Color]
      , initialPos :: Vector
      , initialDir :: Vector
    } deriving (Show)

-- | Function to convert string representation of map into an array
parseMap :: [String] -> Map
parseMap rows =
    A.array ((0,0), (w-1,h-1)) (concat (zipWith parseRow [0..] rows))
    where
    w = length (head rows)
    h = length rows
    parseRow j row = zipWith (parseCell j) [0..] row
    parseCell j i cell = ((i,j), (symbolToObject cell))

{- Helping functions for map parse -}

symbolToObject :: Char -> GameObject
symbolToObject '|' = Wall
symbolToObject '.' = Floor
symbolToObject '$' = Exit
symbolToObject '%' = Entrance
symbolToObject char = colorObject char

colorObject :: Char -> GameObject
colorObject 'a' = Door yellow
colorObject 'b' = Door magenta
colorObject 'c' = Door blue
colorObject 'd' = Door green
colorObject 'e' = Door azure
colorObject 'A' = Button yellow
colorObject 'B' = Button magenta
colorObject 'C' = Button blue
colorObject 'D' = Button green
colorObject 'E' = Button azure
colorObject _ = Floor




-- Width should be odd and not less then 5
-- Height should be odd should be not less then 5
-- generateGrid1 :: Int -> Int -> Int -> [String]
generateGrid1 index width height
    | index == height = [] 
    | otherwise = [generateRow index width height] 
        ++ generateGrid1 (index+1) width height   

generateRow :: Int -> Int -> Int -> String
generateRow index width height
    | isEven index = concat $ replicate width "|"
    | otherwise = "|" ++ repled ++ ".|"
        where 
            repled = (concat $ replicate ((width - 2) `div` 2 ) ".|" )

isEven :: Int -> Bool
isEven n = n `rem` 2 == 0

-- generateMap :: RandomGen g => g -> Level
-- generateMap g = Level {}
--     where
--         m = maze g
--         area = getAreaFromMaze m
--         inds = generateIndexRange area
--         newGridMaze = mazeToGrid inds m []
--         modifiedGridMaze

splitEvery ::Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
    where
    (first,rest) = splitAt n list

joinString :: [[String]] -> String
joinString [] = ""
joinString (x:xs) = x!!0 ++ joinString xs


addBorders :: [String] -> [String]
addBorders curMap = map verticalBorder (lowerHorizontalBorder ++ curMap ++ upperHorizontalBorder)
  where
    upperHorizontalBorder = [concat $ ((replicate (width-1) "|")++["$"] )]
    lowerHorizontalBorder = [concat $ (["%"]++(replicate (width-1) "|"))]
    verticalBorder x =  "|" ++ x ++ "|"
    width = length (head curMap)

-- toBadGrid :: Int -> Int -> [[String]] -> String -> [[]] -> [[String]]
-- toBadGrid width height arr cur res
--     |

-- 11x21