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
--
import System.Random
import MazeGenerator
import Space

-- | Game state
data State = State
  { worldMap :: !Map -- Current level map
  , insideButton :: !Bool -- Bool to check if player is inside a button
  , openDoorsColors :: ![Color] -- List colors of open doors
  , playerPos :: !Vector -- Current player position
  , playerDir :: !Vector -- Current player direction
  , keysPressed :: !(S.Set T.Text) -- Currently pressed button
  }
  deriving (Show)


-- | Function to check if player reached exit of this level
exitReached :: Map -> Vector -> Bool
exitReached m (x, y)
    = candidate1 == Exit || candidate2 == Exit
    where
        candidate1 = m A.! (floor x, floor y)
        candidate2 = m A.! (ceiling (x - 0.99), ceiling (y - 0.99))


-- | Function to handle users interactions
handle :: Event -> State -> State
handle e w@(State {..}) = handle' e
 where
  handle' (TimePassing dt) =
    w  { playerPos = decPos
       , openDoorsColors = newOpenDoorsColors
       , playerDir = newDir
       , insideButton = newInsideButton
       }
    where
      speed = normalized $
        (keyToDir "W" playerDir)
        `vectorSum` (keyToDir "S" (scaledVector (-1) playerDir))
        `vectorSum` (keyToDir "A" (rotatedVector (pi/2) playerDir))
        `vectorSum` (keyToDir "D" (rotatedVector (-pi/2) playerDir))
      newPos = playerPos `vectorSum` scaledVector (2*dt) speed
      newOpenDoorsColors = if newInsideButton && (not insideButton)
                           then getNewDoorsColors newPos worldMap openDoorsColors
                           else openDoorsColors
      newDir = if S.member "Right" keysPressed
               then rotatedVector (-0.05) playerDir
               else if S.member "Left" keysPressed
                    then rotatedVector (0.05) playerDir
                    else playerDir
      newInsideButton = isInsideButton newPos worldMap
      decPos = if (canMove newPos newOpenDoorsColors worldMap) then newPos else playerPos 
      keyToDir k dir =
        if S.member k keysPressed then dir else (0,0)
  handle' (PointerMovement (x, _)) =
        w { playerDir = rotatedVector (-x * pi / 10) (0, 1) }
  handle' (KeyPress k) = w { keysPressed = S.insert k keysPressed }
  handle' (KeyRelease k) = w { keysPressed = S.delete k keysPressed }
  handle' _ = w

-- | Function to check if the movement to the new position is possible
canMove :: Vector -> [Color] -> Map -> Bool
canMove (x,y) openDoorsColors m = canMove' candidate1 && canMove' candidate2
    where
        candidate1 = m A.! (floor x, floor y)
        candidate2 = m A.! (ceiling (x - 0.99), ceiling (y - 0.99))
        canMove' Wall = False
        canMove' Entrance = False
        canMove' Floor = True
        canMove' Exit = True
        canMove' (Door color) = color `elem` openDoorsColors
        canMove' (Button _) = True

-- | Side of ray hit 
data HitSide = Inside | N | S | E | W
  deriving Show

-- | Function to get intersections of ray with vertical grid lines
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

-- | Get collision with object data
collision
  :: Map
  -> Vector -- starting point
  -> Vector -- camera direction
  -> Vector -- ray direction
  -> (GameObject -> Bool)
  -> (HitSide, GameObject, Double {- distance -})
collision m pos cameraDir rayDir isSeen =
  head 
  $ filter isWall
  $ map convert
  $ cellsVisitedByRay pos rayDir
 where
  convert (side, coord, d) =
    (side, m A.! coord, d * cos (angleBetween cameraDir rayDir))
  isWall (_, gameObj, _) = isSeen gameObj

isNotButton :: GameObject -> Bool
isNotButton Floor = False
isNotButton (Button _) = False
isNotButton _ = True

isButton :: GameObject -> Bool
isButton Floor = False
isButton _ = True

-- | Function to draw floor
drawFloor:: Picture
drawFloor = translated 0 7.5 (colored black (solidRectangle sWidth (sHeight)))
  where 
    sWidth = 20.25
    sHeight = fromIntegral 15

-- | Function to draw ceiling
drawCeiling :: Picture
drawCeiling = translated 0 (-7.5) (colored brown (solidRectangle sWidth (sHeight)))
  where 
    sWidth = 20.25
    sHeight = fromIntegral 15

-- | Function to render environment given state
render :: State -> Picture
render state@(State{..}) = hud state {worldMap = newMap} & world state {worldMap = newMap} <> drawFloor <> drawCeiling
       where
          newMap = A.listArray (A.bounds worldMap) (concat dd)
          dd = map (\i -> map (\j ->  toNewMap (worldMap A.! (i,j))) [0..h]) [0..w]
          toNewMap (Door color) = if color `elem` openDoorsColors then Floor else (Door color)
          toNewMap x = x
          ((0,0), (w,h)) = A.bounds worldMap

-- | Function to draw the 3d environment
world :: State -> Picture
world State{..} =
  scaled ratio ratio ((renderButtons worldMap
                                     openDoorsColors
                                     playerPos
                                     playerDir)
                       <> (walls worldMap playerPos playerDir))
 where
  ratio = 20 / i2d screenWidth

-- | Function to render buttons
renderButtons:: Map -> [Color]-> Point -> Vector -> Picture
renderButtons m openDoors pos dir =
  pictures (map door [-halfScreenWidth .. halfScreenWidth])
 where
  door i =
    let (hitSide, objType, distance) = collision m pos dir (rayDir i) isButton
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = shadowedObjectColor hitSide objType
    in case objType of
       (Button bc) -> (colored color $ thickPolygon 0.3
                                                   [(x-0.5, -(y/5)), (x+0.5, -(y/5)), (x+0.5, (y/5)), (x-0.5, (y/5))])
                      <> (colored (getSecondColor bc) $ thickPolyline 1.05 [(x, -(y/5)), (x, (y/5))])
       _ -> blank
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir
  getSecondColor c | c `elem` openDoors = white
                   | otherwise = black

-- | Function to render walls
walls :: Map -> Point -> Vector -> Picture
walls m pos dir =
  pictures (map wallSlice [-halfScreenWidth .. halfScreenWidth])
 where
  wallSlice i =
    let (hitSide, objType, distance) = collision m pos dir (rayDir i) isNotButton
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = shadowedObjectColor hitSide objType
    in case objType of
       (Button _) -> blank
       _ -> colored color $ thickPolyline 1.05 [(x, -y), (x, y)]
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir

-- | Function to render primitive shadows based on the side where rays hit
shadowedObjectColor :: HitSide -> GameObject -> Color
shadowedObjectColor hitSide objType = colorModifier hitSide (objectColor objType)
 where
  colorModifier E = dark
  colorModifier W = dark
  colorModifier _ = id

-- | Function to render hud
hud :: State -> Picture
hud state = translated (-9) 6 $ scaled 0.2 0.2 $ minimap state

-- | Function to render minimap
minimap :: State -> Picture
minimap State{..} =
  player & pictures [cell i j | i <- [0..w], j <- [0..h]]
 where
  (_, (w, h)) = A.bounds worldMap
  cell i j = translated (i2d i) (i2d j) 
             $ drawCell (worldMap A.! (i,j))
  player = uncurry translated playerPos 
           $ colored red 
           $ (solidCircle 0.5 & polyline [(0,0), playerDir])
  drawCell (Button color) = (colored (objectColor (Button color)) $ solidCircle 0.5)
                            <> (colored white $ solidRectangle 1 1)
  drawCell obj = colored (objectColor obj) $ solidRectangle 1 1


-- | Wrapper for activityOf args
data ActivityOf world = ActivityOf
    world
    (Event -> world -> world)
    (world -> Picture)

-- | Current level
data LevelState world = Running Int world

-- | Mapping arguments to activityOf function
runInteraction :: ActivityOf s -> IO ()
runInteraction (ActivityOf state0 handle draw)
    = activityOf state0 handle draw

-- | Turn an interactive program into one with multiple levels.
withManyLevels
    :: RandomGen g => g
    -> (g -> Int -> level) 
    -> (level -> world)
    -> (world -> Bool)
    -> ActivityOf world -- ^ 'interactionOf'.
    -> ActivityOf (LevelState world)
withManyLevels
    gen genLev toWorld isLevelComplete (ActivityOf state0 handle draw)
    = ActivityOf state0' handle' draw'
        where
            state0' = Running 0 (toWorld (genLev gen (0)))
            handle' s (Running levelNumber state)
                = if isLevelComplete state
                  then Running (levelNumber + 1) (toWorld (genLev gen (levelNumber + 1)))
                  else Running levelNumber (handle s state)
            draw' (Running levelNumber state) = draw state

-- | Loading level into the state
levelToState :: Level -> State
levelToState Level{..} = State { worldMap = parseMap levelMap
                                , insideButton = False
                                , openDoorsColors = openColors
                                , playerPos = initialPos
                                , playerDir = initialDir
                                , keysPressed = S.empty
                                }

-- | Is current level complete given some game 'State'?
isLevelComplete :: State -> Bool
isLevelComplete State{..} = exitReached worldMap playerPos

-- | Mapping object to its color 
objectColor :: GameObject -> Color
objectColor Wall = gray
objectColor Floor = white
objectColor Exit = aquamarine
objectColor Entrance = red
objectColor (Door color) = color
objectColor (Button color) = color

-- | Initial ActivityOf
coreActivity :: ActivityOf State
coreActivity = ActivityOf
                   (levelToState (head levels))
                   handle
                   render

{- MAIN -}

getLevelArr :: Level -> [String]
getLevelArr Level {..} = levelMap


getSplitNum:: Area -> Int
getSplitNum (Area (x1, y1) (x2,y2)) = y2 + 1

generateLevel :: RandomGen g => g -> Int -> Level
generateLevel gen lvlNum = Level { 
            levelMap = (addBorders stuff),
            openColors = [],
            initialPos = (1.5,1.5),
            initialDir = (0, 0)}
  where
    maze = generateDifficultLevel gen lvlNum
    area = getAreaFromMaze maze
    inds = generateIndexRange area
    stuff = map joinString (splitEvery (getSplitNum area) (mazeToGrid inds maze []))--


game :: IO ()
game = do 
  g <- newStdGen
  runInteraction (withManyLevels g generateLevel levelToState isLevelComplete coreActivity)
