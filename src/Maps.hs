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

-- | Data type describing all possible objects in the game
data GameObject = Wall
                | Door Color
                | Button Color
                | Floor
                | Entrance
                | Exit
                | Battery
                | Border
        deriving (Show, Eq)

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
symbolToObject '#' = Battery
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
