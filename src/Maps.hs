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

testMap ::[String]
testMap =
    [ "111111111111111111111111"
    , "100000100000000010000001"
    , "100000100022000010000001"
    , "100300100022000010000001"
    , "100000100000000010000001"
    , "100000100000000010000001"
    , "100111111000011110000001"
    , "100000000000000000000001"
    , "100000000000000000000001"
    , "100030000003000000030001"
    , "100000000000000000000001"
    , "100000000000000000000001"
    , "111111111111111111111111"
    ]