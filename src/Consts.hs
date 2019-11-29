{- Constants -}
module Consts where

-- | Screen dimensions in pixelx
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


-- | Battery
batteryMax, batteryMin :: Double
batteryMax = 100.0
batteryMin = 0.0

-- | How fast the battery loses energy
dischargingRate :: Double
dischargingRate = 5.0

-- | constants for minimap
cellRad, cellSize, stripeWidth :: Double
cellSize = 1.05
stripeWidth = 1.15
cellRad = 0.5

-- | Screen dimensions in CodeWorld values
width, height :: Double
width = 20.05
height = 30