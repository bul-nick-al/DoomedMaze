{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module DoomedMaze
    ( game
    ) where

import CodeWorld
import Maps
import Consts
import Vectors
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String


{- GAME STATE -}

data State = State
  { levelNumber :: !Int
  , worldMap :: !Map
  , playerPos :: !Vector
  , playerDir :: !Vector
  , keysPressed :: !(S.Set T.Text)
  }
  deriving (Show)

{- EVENT HANDLING -}

exitReached :: Map -> Vector -> Bool
exitReached m (posX, posY) = (m A.! (round posX,round posY)) == 4


handle :: Event -> State -> State
handle e w@(State {..}) = handle' e
 where
  handle' (TimePassing dt) =
    if exitReached (worldMap) (playerPos)
    then (State
               { levelNumber = levelNumber + 1
               , worldMap = parseMap (levels !! (levelNumber + 1))
               , playerPos = (1.5,1.5)
               , playerDir = (1,1)
               , keysPressed = S.empty
               })
   else w  { playerPos = decPos }
    where
      speed = normalized $
        (keyToDir "W" playerDir)
        `vectorSum` (keyToDir "S" (scaledVector (-1) playerDir))
        `vectorSum` (keyToDir "A" (rotatedVector (pi/2) playerDir))
        `vectorSum` (keyToDir "D" (rotatedVector (-pi/2) playerDir))
      newPos = playerPos `vectorSum` scaledVector (2*dt) speed
      decPos = if (canMove newPos worldMap) then  newPos else playerPos
      keyToDir k dir =
        if S.member k keysPressed then dir else (0,0)
  handle' (PointerMovement (x, _)) =
      w { playerDir = rotatedVector (-x * pi / 10) (0, 1) }
  handle' (KeyPress k) = w { keysPressed = S.insert k keysPressed }
  handle' (KeyRelease k) = w { keysPressed = S.delete k keysPressed }
  handle' _ = w

canMove :: Vector -> Map -> Bool
canMove (x,y) world = ((world A.! (round x,round y)) /= 1) 
  && ((world A.! (floor x,floor y)) /= 1)
  
{- RAY CASTING -}

data HitSide = Inside | N | S | E | W
  deriving Show

-- from http://www.cse.yorku.ca/~amana/research/grid.pdf
cellsVisitedByRay 
  :: Vector -- starting point 
  -> Vector -- direction
  -> [(HitSide, (Int, Int), Double)]
cellsVisitedByRay (posX, posY) (dirX, dirY) =
  (Inside, (initI, initJ), 0) : go initI initJ initTMaxX initTMaxY 
 where
  initI = floor posX
  initJ = floor posY
  stepI = if dirX > 0 then 1 else -1
  stepJ = if dirY > 0 then 1 else -1
  tDeltaX = abs (1 / dirX)
  tDeltaY = abs (1 / dirY)
  xSide = if dirX > 0 then W else E
  ySide = if dirY > 0 then S else N
  initTMaxX =
    if dirX > 0
      then (1 + i2d initI - posX) * tDeltaX
      else (posX - i2d initI) * tDeltaX
  initTMaxY =
    if dirY > 0
      then (1 + i2d initJ - posY) * tDeltaY
      else (posY - i2d initJ) * tDeltaY
  go i j tMaxX tMaxY
    | tMaxX < tMaxY =
        let i' = i + stepI
            tMaxX' = tMaxX + tDeltaX
        in (xSide, (i', j), tMaxX) : go i' j tMaxX' tMaxY
    | otherwise =
        let j' = j + stepJ
            tMaxY' = tMaxY + tDeltaY
        in (ySide, (i, j'), tMaxY) : go i j' tMaxX tMaxY'

collision
  :: Map
  -> Vector -- starting point
  -> Vector -- camera direction
  -> Vector -- ray direction
  -> (HitSide, WallType, Double {- distance -})
collision m pos cameraDir rayDir =
  head 
  $ filter isWall
  $ map convert
  $ cellsVisitedByRay pos rayDir
 where
  convert (side, coord, d) =
    (side, m A.! coord, d * cos (angleBetween cameraDir rayDir))
  isWall (_, wallType, _) = wallType > 0

{- RENDERING -}

drawFloor:: Picture
drawFloor = translated 0 7.5 (colored black (solidRectangle sWidth (sHeight)))
  where 
    sWidth = fromIntegral 20
    sHeight = fromIntegral 15

drawCeiling :: Picture
drawCeiling = translated 0 (-7.5) (colored brown (solidRectangle sWidth (sHeight)))
  where 
    sWidth = fromIntegral 20
    sHeight = fromIntegral 15

render :: State -> Picture
render state = lettering (fromString (show (exitReached (worldMap state) (playerPos state))))
                <> hud state & world state <> drawFloor <> drawCeiling

world :: State -> Picture
world State{..} =
  scaled ratio ratio (walls worldMap playerPos playerDir)
 where
  ratio = 20 / i2d screenWidth

walls :: Map -> Point -> Vector -> Picture
walls m pos dir =
  pictures (map wallSlice [-halfScreenWidth .. halfScreenWidth])
 where
  wallSlice i =
    let (hitSide, wallType, distance) = collision m pos dir (rayDir i)
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = wallColor hitSide wallType
    in colored color $ thickPolyline 1 [(x, -y), (x, y)]
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir

wallColor :: HitSide -> WallType -> Color
wallColor hitSide wallType = colorModifier hitSide (minimapColor wallType)
 where
  colorModifier E = dark
  colorModifier W = dark
  colorModifier _ = id

hud :: State -> Picture
hud state = translated (-9) 7 $ scaled 0.2 0.2 $ minimap state

minimap :: State -> Picture
minimap State{..} = 
  player & pictures [cell i j | i <- [0..w], j <- [0..h]]
 where
  (_, (w, h)) = A.bounds worldMap
  cell i j = translated (i2d i) (i2d j) 
             $ colored (minimapColor (worldMap A.! (i,j)))
             $ solidRectangle 1 1
  player = uncurry translated playerPos 
           $ colored red 
           $ (solidCircle 0.5 & polyline [(0,0), playerDir])

minimapColor :: WallType -> Color
minimapColor 0 = white
minimapColor 1 = grey
minimapColor 2 = blue
minimapColor 3 = green
minimapColor 4 = red
minimapColor _ = black

{- MAIN -}

game :: IO ()
game = do
  activityOf
    (State
      {
        levelNumber = 0
      , worldMap = parseMap (levels !! 0)
      , playerPos = (1.5,1.5)
      , playerDir = (1,1)
      , keysPressed = S.empty
      })
    handle
    render
