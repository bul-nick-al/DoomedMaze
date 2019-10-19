{- VECTORS -}
module Vectors where

import CodeWorld

i2d :: Int -> Double
i2d = fromIntegral

normalized :: Vector -> Vector
normalized v = if len < 1e-6 then (0,0) else scaledVector (1 / len) v
 where
  len = vectorLength v

angleBetween :: Vector -> Vector -> Double
angleBetween u@(ux, uy) v@(vx, vy) = atan2 det dot
 where
  det = ux * vy - vx * uy
  dot = dotProduct u v