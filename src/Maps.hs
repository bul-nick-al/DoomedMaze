module Maps where


import CodeWorld
import Vectors
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String
import Doors

type WallType = Int
type Map = A.Array (Int, Int) WallType

data Level = Level {
        levelMap :: [String]
      , initialDoors :: [Door]  
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

adjustMapToDoors :: [Int] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
adjustMapToDoors closedDoors wMap = map needsChange wMap
  where
    needsChange ((xCor,yCor),col) = if col `notElem` closedDoors && notWall col then ((xCor,yCor),0) else ((xCor,yCor),col)
    notWall e = e /= 1 && e /= 4

-- | A list of all level maps.
levels :: [Level]
levels = [
    level1,
    level2,
    level3
    ]


level3 :: Level
level3 = Level {
        levelMap = [  "111111111111111111111111"
                    , "100000100000000010000001"
                    , "100000100022000010000001"
                    , "100700100022000010000001"
                    , "100300100000000010004001"
                    , "100000100000000010000001"
                    , "100111111000011117777771"
                    , "100700000000000000000001"
                    , "100000000000000000000001"
                    , "100030000003000000030001"
                    , "100000000000000000000001"
                    , "100000000000000000000001"
                    , "111111111111111111111111"
                   ]
        , initialDoors = [(Door (3,3) 7), (Door (3,4) 3)]
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
        , initialDoors = []
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }


level1 :: Level
level1 = Level {
          levelMap = [
                     "111111111111111111111",
                     "101400030010000000101",
                     "101111111010161110101",
                     "106000001010100010601",
                     "101111101010111011101",
                     "100010131010001010501",
                     "101710101011111510111",
                     "151050101000000010101",
                     "121010101111111110101",
                     "101010100000000010131",
                     "131116111711111010101",
                     "131210000400101010601",
                     "101011111117101211101",
                     "101000000010100010001",
                     "101111101110101110111",
                     "101000001000100070101",
                     "101011111511111110101",
                     "101010001000000010101",
                     "101611101011111010151",
                     "100000003000001000001",
                     "111111111111111111111"
                     ]
        , initialDoors = [(Door (1,9) 3)]
        , initialPos = (1.5,1.5)
        , initialDir = (1, 1)
    }

