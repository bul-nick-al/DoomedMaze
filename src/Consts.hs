{- Constants -}
module Consts where


screenWidth, screenHeight :: Int
screenWidth = 80
screenHeight = 60

fov :: Double
fov = pi / 4

halfScreenWidth, halfScreenHeight :: Int
halfScreenWidth = screenWidth `div` 2
halfScreenHeight = screenHeight `div` 2
