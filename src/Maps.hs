module Maps where


import qualified Data.Array as A

type WallType = Int
type Map = A.Array (Int, Int) WallType

parseMap :: [String] -> Map
parseMap rows =
    A.array ((0,0), (w-1,h-1)) (concat (zipWith parseRow [0..] rows))
    where
    w = length (head rows)
    h = length rows
    parseRow j row = zipWith (parseCell j) [0..] row
    parseCell j i cell = ((i,j), read [cell])


-- | A list of all level maps.
levels :: [[String]]
levels = [
    level1,
    level2
    ]


level1 ::[String]
level1 =
    [ "111111111111111111111111"
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


level2 ::[String]
level2 =
    [ "111111111111111111111111"
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
