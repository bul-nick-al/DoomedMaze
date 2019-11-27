{- Constants -}
module Consts where

-- | Screen dimensions
screenWidth, screenHeight :: Int
screenWidth = 210
screenHeight = 180

-- | Field of view
fov :: Double
fov = pi / 4

-- | Screen dimensions halved. 
--    (Used due to other coordinate system in CodeWorld)
halfScreenWidth, halfScreenHeight :: Int
halfScreenWidth = screenWidth `div` 2
halfScreenHeight = screenHeight `div` 2
