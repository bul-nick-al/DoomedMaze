module Maps where


import CodeWorld
import Vectors
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String

data GameObject = Wall | Door Color | Button Color | Floor | Exit deriving (Show, Eq)
type Map = A.Array (Int, Int) GameObject

-- | Level data type
data Level = Level {
        levelMap :: [String]
      , openColors :: [Color]
      , initialPos :: Vector
      , initialDir :: Vector
    }

-- | Function to convert stringed map into the array 
parseMap :: [String] -> Map
parseMap rows =
    A.array ((0,0), (w-1,h-1)) (concat (zipWith parseRow [0..] rows))
    where
    w = length (head rows)
    h = length rows
    parseRow j row = zipWith (parseCell j) [0..] row
    parseCell j i cell = ((i,j), (symbolToObject cell))

{- Helping function for map parse -}

symbolToObject :: Char -> GameObject
symbolToObject '|' = Wall
symbolToObject '.' = Floor
symbolToObject '$' = Exit
symbolToObject char = colorObject char

colorObject :: Char -> GameObject
colorObject 'a' = Door yellow
colorObject 'b' = Door grey
colorObject 'c' = Door blue
colorObject 'd' = Door green
colorObject 'e' = Door red
colorObject 'f' = Door azure
colorObject 'A' = Button yellow
colorObject 'B' = Button grey
colorObject 'C' = Button blue
colorObject 'D' = Button green
colorObject 'E' = Button red
colorObject 'F' = Button azure
colorObject _ = Floor


-- | A list of all level maps.
levels :: [Level]
levels = [
    level1,
    level2,
    level3
    ]


level3 :: Level
level3 = Level {
        levelMap = [  "||||||||||||||||||||||||"
                    , "|.....|.........|......|"
                    , "|.....|...bb....|......|"
                    , "|..F..|...bb....|......|"
                    , "|..D..|.........|...$..|"
                    , "|.....|.........|......|"
                    , "|..||||||....||||ffffff|"
                    , "|..f...................|"
                    , "|......................|"
                    , "|...d......d.......d...|"
                    , "|......................|"
                    , "|......................|"
                    , "||||||||||||||||||||||||"
                   ]

        , openColors = []
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }


level2 :: Level
level2 = Level {
          levelMap = [ "||||||||||||||||||||||||"
                     , "|.....|.........|......|"
                     , "|.....|...bb....|......|"
                     , "|..D..|...bb....|......|"
                     , "|.....|.........|...$..|"
                     , "|.....|.........|......|"
                     , "|............ddd|......|"
                     , "|.......d..............|"
                     , "|.......d..............|"
                     , "|.......d..........d...|"
                     , "|.......d..............|"
                     , "|......................|"
                     , "||||||||||||||||||||||||"
                     ]
        , openColors = []
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }


level1 :: Level
level1 = Level {
          levelMap = [
                     "|||||||||||||||||||||",
                     "|.|A...F..|.......|.|",
                     "|.|||||||.|.|f|||.|.|",
                     "|.f.....|.|.|...|.f.|",
                     "|.|||||.|.|.|||.|||.|",
                     "|...|.|F|.|...|.|.d.|",
                     "|.|a|.|.|.|||||d|.|||",
                     "|d|.d.|.|.......|.|.|",
                     "|D|.|.|.|||||||||.|.|",
                     "|.|.|.|.........|.|F|",
                     "|F|||f|||a|||||.|.|.|",
                     "|f|D|....$..|.|.|.f.|",
                     "|.|.|||||||a|.|D|||.|",
                     "|.|.......|.|...|...|",
                     "|.|||||.|||.|.|||.|||",
                     "|.|.....|...|...a.|.|",
                     "|.|.|||||d|||||||.|.|",
                     "|.|.|...|.......|.|.|",
                     "|.|f|||.|.|||||.|.|d|",
                     "|.......F.....|.....|",
                     "|||||||||||||||||||||"
                     ]
        , openColors = [green]
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }
                      