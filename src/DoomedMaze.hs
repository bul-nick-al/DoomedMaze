{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module DoomedMaze
    ( game
    ) where

import CodeWorld
import Maps
import Consts
import Vectors
import Doors
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String


{- GAME STATE -}

data State = State
  { worldMap :: !Map
  , doors :: ![Door]
  , playerPos :: !Vector
  , playerDir :: !Vector
  , keysPressed :: !(S.Set T.Text)
  }
  deriving (Show)

{- EVENT HANDLING -}

exitReached :: Map -> Vector -> Bool
exitReached m (posX, posY)
    = ((m A.! (ceiling (posX - 0.99) ,ceiling (posY - 0.99))) == 4)
    && ((m A.! (floor posX, floor posY)) == 4)


handle :: Event -> State -> State
handle e w@(State {..}) = handle' e
 where
  handle' (TimePassing dt) =
    w  { playerPos = decPos, doors = newDoors, worldMap = newMap, playerDir = newDir }
    where
      speed = normalized $
        (keyToDir "W" playerDir)
        `vectorSum` (keyToDir "S" (scaledVector (-1) playerDir))
        `vectorSum` (keyToDir "A" (rotatedVector (pi/2) playerDir))
        `vectorSum` (keyToDir "D" (rotatedVector (-pi/2) playerDir))
      newPos = playerPos `vectorSum` scaledVector (2*dt) speed
      newDoors = getNewDoors newPos doors
      newDir = if S.member "Right" keysPressed
               then rotatedVector (-0.05) playerDir
               else if S.member "Left" keysPressed
                    then rotatedVector (0.05) playerDir
                    else playerDir
      closedDoors = getClosedDoorColors newDoors
      newMap = A.listArray (A.bounds worldMap) newAss
        where
          newAssocs = adjustMapToDoors closedDoors (A.assocs worldMap)
          newAss = map getCols newAssocs
          getCols (x,y) = y
      decPos = if (canMove newPos closedDoors worldMap) then  newPos else playerPos --
      keyToDir k dir =
        if S.member k keysPressed then dir else (0,0)
--  handle' (KeyPress "Left") = w {playerDir = rotatedVector (0.2) playerDir}
--  handle' (KeyPress "Right") = w {playerDir = rotatedVector (-0.2) playerDir}
  handle' (PointerMovement (x, _)) =
        w { playerDir = rotatedVector (-x * pi / 10) (0, 1) }
  handle' (KeyPress k) = w { keysPressed = S.insert k keysPressed }
  handle' (KeyRelease k) = w { keysPressed = S.delete k keysPressed }
  handle' _ = w


canMove :: Vector -> [Int] -> Map -> Bool
canMove (x,y) closedDoors world = ((world A.! (ceiling (x - 0.99) ,ceiling (y - 0.99))) `notElem` prohTiles)
  && ((world A.! (floor x,floor y)) `notElem` prohTiles)
    where
      prohTiles = [1] ++ closedDoors --

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
  -> (Int -> Bool)
  -> (HitSide, WallType, Double {- distance -})
collision m pos cameraDir rayDir isSeen =
  head 
  $ filter isWall
  $ map convert
  $ cellsVisitedByRay pos rayDir
 where
  convert (side, coord, d) =
    (side, m A.! coord, d * cos (angleBetween cameraDir rayDir))
  isWall (_, wallType, _) = isSeen wallType

isWall :: Int -> Bool
isWall wallType = wallType > 0 && wallType /= 3

isDoor :: Int -> Bool
isDoor wallType = wallType > 0
{- RENDERING -}


drawFloor:: Picture
drawFloor = translated 0 7.5 (colored black (solidRectangle sWidth (sHeight)))
  where 
    sWidth = 20.25
    sHeight = fromIntegral 15

drawCeiling :: Picture
drawCeiling = translated 0 (-7.5) (colored brown (solidRectangle sWidth (sHeight)))
  where 
    sWidth = 20.25
    sHeight = fromIntegral 15

render :: State -> Picture
render state@(State{..}) =  lettering (fromString (show (doors))) <> (translated 0 (-3) (lettering (fromString(show playerPos))))
                <> hud state & world state <> drawFloor <> drawCeiling

world :: State -> Picture
world State{..} =
  scaled ratio ratio ((renderDoors worldMap playerPos playerDir) <> (walls worldMap playerPos playerDir))
 where
  ratio = 20 / i2d screenWidth


renderDoors:: Map -> Point -> Vector -> Picture
renderDoors m pos dir =
  pictures (map door [-halfScreenWidth .. halfScreenWidth])
 where
  door i =
    let (hitSide, wallType, distance) = collision m pos dir (rayDir i) isDoor
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = wallColor hitSide wallType
    in if wallType == 3
       then (colored color $ thickPolygon 0.3 [(x-0.5, -(y/5)), (x+0.5, -(y/5)), (x+0.5, (y/5)), (x-0.5, (y/5))])
            <> (colored black $ thickPolyline 1.05 [(x, -(y/5)), (x, (y/5))])
       else blank
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir

walls :: Map -> Point -> Vector -> Picture
walls m pos dir =
  pictures (map wallSlice [-halfScreenWidth .. halfScreenWidth])
 where
  wallSlice i =
    let (hitSide, wallType, distance) = collision m pos dir (rayDir i) isWall
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = wallColor hitSide wallType
    in if wallType /= 3
           then colored color $ thickPolyline 1.05 [(x, -y), (x, y)]
           else blank
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir

wallColor :: HitSide -> WallType -> Color
wallColor hitSide wallType = colorModifier hitSide (minimapColor wallType)
 where
  colorModifier E = dark
  colorModifier W = dark
  colorModifier _ = id

hud :: State -> Picture
hud state = translated (-9) 6 $ scaled 0.2 0.2 $ minimap state

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



data ActivityOf world = ActivityOf
    world
    (Event -> world -> world)
    (world -> Picture)

data LevelState world = Running Int world

runInteraction :: ActivityOf s -> IO ()
runInteraction (ActivityOf state0 handle draw)
    = activityOf state0 handle draw

-- | Turn an interactive program into one with multiple levels.
withManyLevels
    :: [level]
    -> (level -> world)
    -> (world -> Bool)
    -> ActivityOf world -- ^ 'interactionOf'.
    -> ActivityOf (LevelState world)
withManyLevels
    levels toWorld isLevelComplete (ActivityOf state0 handle draw)
    = ActivityOf state0' handle' draw'
        where
            state0' = Running 0 (toWorld (levels !! 0))
            handle' s (Running levelNumber state)
                = if isLevelComplete state
                  then Running (levelNumber + 1) (toWorld (levels !! (levelNumber + 1)))
                  else Running levelNumber (handle s state)
            draw' (Running levelNumber state) = draw state


levelToState :: Level -> State
levelToState Level{..} = State { worldMap = parseMap levelMap
                                , doors = initialDoors
                                , playerPos = initialPos
                                , playerDir = initialDir
                                , keysPressed = S.empty
                                }

-- | Is current level complete given some game 'State'?
isLevelComplete :: State -> Bool
isLevelComplete State{..} = exitReached worldMap playerPos






minimapColor :: WallType -> Color
minimapColor 0 = white
minimapColor 1 = grey
minimapColor 2 = blue
minimapColor 3 = green
minimapColor 4 = red
minimapColor 7 = azure
minimapColor _ = black


coreActivity :: ActivityOf State
coreActivity = ActivityOf
                   (levelToState (levels !! 0))
                   handle
                   render

{- MAIN -}

game :: IO ()
game = do 
  runInteraction (withManyLevels levels levelToState isLevelComplete coreActivity)
