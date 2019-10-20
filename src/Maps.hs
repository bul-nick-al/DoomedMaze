module Maps where


import CodeWorld
import Vectors
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String
type WallType = Int
type Map = A.Array (Int, Int) WallType

data Level = Level {
        levelMap :: [String]
      , initialPos :: Vector
      , initialDir :: Vector
    }

parseMap :: [String] -> Map
parseMap rows =
    A.array ((0,0), (w-1,h-1)) (concat (zipWith parseRow [0..] rows))
    where
    w = length (head rows)
    h = length rows
    parseRow j row = zipWith (parseCell j) [0..] row
    parseCell j i cell = ((i,j), read [cell])


-- | A list of all level maps.
levels :: [Level]
levels = [
    level1,
    level2
    ]


level1 :: Level
level1 = Level {
        levelMap = [ "111111111111111111111111"
                    , "100000100000000010000001"
                    , "100000100022000010000001"
                    , "100300100022000010000001"
                    , "100000100000000010004001"
                    , "100000100000000010000001"
                    , "100111111000011110000001"
                    , "100000000000000000000001"
                    , "100000000000000000000001"
                    , "100030000003000000030001"
                    , "100000000000000000000001"
                    , "100000000000000000000001"
                    , "111111111111111111111111"
                   ]
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }


level2 :: Level
level2 = Level {
          levelMap = [ "111111111111111111111111"
                     , "100000100000000010000001"
                     , "100000100022000010000001"
                     , "100300100022000010000001"
                     , "100000100000000010004001"
                     , "100000100000000010000001"
                     , "100000000000033310000001"
                     , "100000003040000000000001"
                     , "100000003000000000000001"
                     , "100000003000000000030001"
                     , "100000003000000000000001"
                     , "100000000000000000000001"
                     , "111111111111111111111111"
                     ]
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }

