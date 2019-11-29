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
import qualified Data.Fixed as F
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
  , energy :: Double
  , showMap :: Bool
  , time :: Double
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
  handle' (TimePassing dt) = handleStateWithTime (TimePassing dt) w
  handle' (PointerMovement (x, _)) =
        w { playerDir = rotatedVector (-x * pi / 10) (0, 1) }
  handle' (KeyPress k) = w { keysPressed = S.insert k keysPressed }
  handle' (KeyRelease k) = w { keysPressed = S.delete k keysPressed }
  handle' _ = w


-- | Update the state as the time passes by
handleStateWithTime :: Event -> State -> State
handleStateWithTime (TimePassing dt) w@(State {..}) =
    w  { playerPos = decPos
       , openDoorsColors = newOpenDoorsColors
       , playerDir = newDir
       , insideButton = newInsideButton
       , energy = newEnergy
       , showMap = newShowMap
       , time = newTime
       , worldMap = newMap
       }
        where
          newPos = calcNewPosition w dt
          newOpenDoorsColors = calcNewOpenDoorsColors w newPos newInsideButton
          newDir = calcNewDir w
          newEnergy = calcNewEnergy w dt pickedBattery
          newShowMap = (S.member "M" keysPressed)
          decPos = if (canMove newPos newOpenDoorsColors worldMap)
                   then newPos
                   else playerPos
          newInsideButton = isInsideButton newPos worldMap
          newTime = time + dt
          (newMap, pickedBattery)= checkBatteries worldMap newPos

-- | Check whether the player has picked a battery and remove the battery from the mazy
checkBatteries :: Map -> Vector -> (Map, Bool)
checkBatteries m pos
    = case (isInsideBattery pos m) of
      (True, Just coords) -> (m A.// [(coords, Floor)], True)
      _ -> (m, False)



-- | Calculates how much energy the player has left based on the state
calcNewEnergy :: State -> Double -> Bool -> Double
calcNewEnergy (State {..}) dt pickedBattery
    | pickedBattery = batteryMax
    | (showMap || S.member "Shift" keysPressed)
      && energy > batteryMin = energy - dt * dischargingRate
    | length keysPressed == 0
      && energy < batteryMax = energy + dt
    | otherwise = energy

-- | Calculate in which direction the player is looking
calcNewDir :: State -> Vector
calcNewDir (State {..})
    | S.member "Right" keysPressed = rotatedVector (-0.05) playerDir
    | S.member "Left" keysPressed = rotatedVector (0.05) playerDir
    | otherwise = playerDir


-- | Makes an array of colors of open doors
calcNewOpenDoorsColors :: State -> Vector -> Bool -> [Color]
calcNewOpenDoorsColors (State {..}) newPos newInsideButton =
    if newInsideButton && (not insideButton)
    then getNewDoorsColors newPos worldMap openDoorsColors
    else openDoorsColors

-- | calculates new position of the player
calcNewPosition :: State -> Double -> Vector
calcNewPosition (State {..}) dt
    = playerPos `vectorSum` scaledVector (2*dt) (accelerate speed acceleration)
         where
            keyToDir k dir =
                if S.member k keysPressed then dir else (0,0)
            speed = normalized $
                                (keyToDir "W" playerDir)
                                `vectorSum` (keyToDir "S" (scaledVector (-1) playerDir))
                                `vectorSum` (keyToDir "A" (rotatedVector (pi/2) playerDir))
                                `vectorSum` (keyToDir "D" (rotatedVector (-pi/2) playerDir))
            acceleration = if S.member "Shift" keysPressed then 2 else 1
            accelerate (a, b) acc = (a * acc, b * acc)


-- | Function to check if the movement to the new position is possible
canMove :: Vector -> [Color] -> Map -> Bool
canMove (x,y) openDoorsColors m = canMove' candidate1 && canMove' candidate2
    where
        candidate1 = getCandidate (floor x, floor y)
        candidate2 = getCandidate (ceiling (x - 0.99), ceiling (y - 0.99))
        canMove' Wall = False
        canMove' Border = False
        canMove' Entrance = False
        canMove' Floor = True
        canMove' Exit = True
        canMove' (Door color) = color `elem` openDoorsColors
        canMove' (Button _) = True
        canMove' Battery = True
        getCandidate coord
                                 | coord `elem` (A.indices m) = m A.! coord
                                 | otherwise = Border

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
    (side, getObj coord, d * cos (angleBetween cameraDir rayDir))
  isWall (_, gameObj, _) = isSeen gameObj
  getObj coord
            | coord `elem` (A.indices m) = m A.! coord
            | otherwise = Border

-- | Get collision with object data
collision2d
  :: Map
  -> Vector -- starting point
  -> Vector -- camera direction
  -> Vector -- ray direction
  -> (GameObject -> Bool) -- the objects to save
  -> (GameObject -> Bool) -- stopping object
  -> [(HitSide, GameObject, Double {- distance -})]
collision2d m pos cameraDir rayDir isSeen shouldStop =
  filter isSeen'
  $ takeWhile shouldStop'
  $ map convert
  $ cellsVisitedByRay pos rayDir
 where
  convert (side, coord, d) =
    (side, getObj coord, d * cos (angleBetween cameraDir rayDir))
  isSeen' (_, gameObj, _) = isSeen gameObj
  shouldStop' (_, gameObj, _) = shouldStop gameObj
  getObj coord
            | coord `elem` (A.indices m) = m A.! coord
            | otherwise = Border


-- | True if an object is an obstacle
isObstacle :: GameObject -> Bool
isObstacle Floor = False
isObstacle (Button _) = False
isObstacle (Battery) = False
isObstacle _ = True

isInteractiveObject :: GameObject -> Bool
isInteractiveObject Floor = False
isInteractiveObject _ = True

-- | Function to draw floor
drawFloor:: Picture
drawFloor = translated 0 7.5 (colored black (solidRectangle sWidth (sHeight)))
  where
    sWidth = 20.05
    sHeight = fromIntegral 15

-- | Function to draw ceiling
drawCeiling :: Picture
drawCeiling = translated 0 (-7.5) (colored brown (solidRectangle sWidth (sHeight)))
  where
    sWidth = 20.05
    sHeight = fromIntegral 15

-- | Function to render environment given state
render :: State -> Picture
render state@(State{..}) = batteryIndicator state
                           & darkness state
                           & hud state {worldMap = newMap}
                           & world state {worldMap = newMap}
                           & drawFloor
                           & drawCeiling
       where
          newMap = A.listArray (A.bounds worldMap) (concat dd)
          dd = map (\i -> map (\j ->  toNewMap (worldMap A.! (i,j))) [0..h]) [0..w]
          toNewMap (Door color) = if color `elem` openDoorsColors then Floor else (Door color)
          toNewMap x = x
          ((0,0), (w,h)) = A.bounds worldMap

-- | Function to draw the 3d environment
world :: State -> Picture
world State{..} =
  scaled ratio ratio ((renderInteractiveObjects worldMap
                                                time
                                                openDoorsColors
                                                playerPos
                                                playerDir)
                       <> (walls worldMap playerPos playerDir))
 where
  ratio = 20 / i2d screenWidth

-- | Function to render buttons and batteries
renderInteractiveObjects :: Map -> Double -> [Color]-> Point -> Vector -> Picture
renderInteractiveObjects m time openDoors pos dir =
  pictures (map door [-halfScreenWidth .. halfScreenWidth])
 where
  door i =
    let (hitSide, objType, distance) = collision m pos dir (rayDir i) isInteractiveObject
        x = i2d i
        y = (i2d halfScreenHeight) / distance
        color = shadowedObjectColor hitSide objType
    in case objType of
       (Button bc) -> renderButton (x, y) color openDoors bc
       Battery -> renderBattery (x, y) time
       _ -> blank

  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir


-- | Function to render a button
renderButton :: Vector -> Color -> [Color] -> Color -> Picture
renderButton (x, y) color openDoors bc
    = (colored color
      $ thickPolygon 0.3
                     [(x-0.5, -(y/5)),
                      (x+0.5, -(y/5)),
                      (x+0.5, (y/5)),
                      (x-0.5, (y/5))])
      <> (colored (getSecondColor bc)
                  $ thickPolyline 1.1
                                  [(x, -(y/5)),
                                  (x, (y/5))])
    where
        getSecondColor c | c `elem` openDoors = white
                           | otherwise = black


-- | Renders a pulsating violate object (which is the battery)
renderBattery :: Vector -> Double -> Picture
renderBattery (x, y) time
    = colored violet
    $ thickPolyline 1.2 [(x, -y * (F.mod' time 1))
                        , (x, y * (F.mod' time 1))]


-- | Makes screen darker depending on the amount of energy
darkness :: State -> Picture
darkness State{..} = (colored (RGBA 0 0 0 (alpha)) $ solidRectangle 20.25 30)
    where
        alpha = (100 - energy)/100


-- | Function to render the battery indicator that shows how much energy is left
batteryIndicator :: State -> Picture
batteryIndicator State{..}
    = translated 9 7 (thickRectangle 0.05 1 3
                      <> translated 0 ((shift - 3) / 2)
                                  (colored (translucent color)
                                           (solidRectangle 1 shift)))
                    where
                        shift = (3 * energy / 100.0)
                        color = if energy < 20 then red else green


-- | Shows the number of the current level
drawLevelNumber :: Int -> Picture
drawLevelNumber number
    = colored white
              (translated (-8) 8
              (lettering (fromString ("Level " <> (show number)))))


-- | Function to render walls
walls :: Map -> Point -> Vector -> Picture
walls m pos dir =
  pictures (map wallSlice [-halfScreenWidth .. halfScreenWidth])
 where
  wallSlice i =
    let (hitSide, objType, distance)
              = collision m pos dir (rayDir i) isObstacle
        x     = i2d i
        y     = (i2d halfScreenHeight) / distance
        color = shadowedObjectColor hitSide objType
    in case objType of
       (Button _) -> blank
       _ -> colored color $ thickPolyline 1.15 [(x, -y), (x, y)]
  rayDir i = rotatedVector (-fov * i2d i / i2d screenWidth) dir

-- | Function to render primitive shadows based on the side where rays hit
shadowedObjectColor :: HitSide -> GameObject -> Color
shadowedObjectColor hitSide objType
    = colorModifier hitSide (objectColor objType)
        where
         colorModifier E = dark
         colorModifier W = dark
         colorModifier _ = id

-- | Function to render hud
hud :: State -> Picture
hud state@(State {..})
    = if showMap
      then translated (-w'/2)
                      (-h'/2) $ scaled scale
                                       scale
                                       $ minimap state
      else blank
         where
            (_, (w, h)) = A.bounds worldMap
            w' = i2d w * scale
            h' = i2d h * scale
            scale = 0.5

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
  drawCell (Button color) = colored (objectColor (Button color))
                                    $ solidCircle 0.5
                          <> (colored white
                                    $ solidRectangle 1.05 1.05)
  drawCell obj = colored (objectColor obj)
                         $ solidRectangle 1.05 1.05


-- | Wrapper for activityOf args
data ActivityOf world = ActivityOf
    world
    (Event -> world -> world)
    (world -> Picture)

-- | Current level
data GameState world = Running world | StartScreen | Paused world
data WithLevel world = WithLevel Int world

-- | The starting screen of the game
startScreen :: Picture
startScreen = lettering "Press Enter to start"

-- | The pause screen of the game
pauseScreen :: Picture
pauseScreen = translated 0 1 (lettering  "Press Enter to continue")
              <> (translated 0 (-1) (lettering "Press ESC to restart"))


-- | Mapping arguments to activityOf function
runInteraction :: ActivityOf s -> IO ()
runInteraction (ActivityOf state0 handle draw)
    = activityOf state0 handle draw

withStartScreen :: ActivityOf s -> ActivityOf (GameState s)
withStartScreen (ActivityOf state0 handle draw)
  = ActivityOf state0' handle' draw'
  where
    state0'                                = StartScreen
    handle' (KeyPress "Enter") StartScreen = Running state0
    handle' (KeyPress "Esc")   (Running s) = Paused s
    handle' (KeyPress "Enter") (Paused s)  = Running s
    handle' (KeyPress "Esc")   (Paused s)  = Running state0
    handle' _                  StartScreen = StartScreen
    handle' _                  (Paused s)  = Paused s
    handle' e                  (Running s) = Running (handle e s)
    draw'   (Paused s)                     = pauseScreen
    draw'   StartScreen                    = startScreen
    draw'   (Running s)                    = draw s

-- | Turn an interactive program into one with multiple levels.
withManyLevels
    :: RandomGen g => g
    -> (g -> Int -> level)
    -> (level -> world)
    -> (world -> Bool)
    -> ActivityOf world -- ^ 'interactionOf'.
    -> ActivityOf (WithLevel world)
withManyLevels
    gen
    genLev
    toWorld
    isLevelComplete
    (ActivityOf state0 handle draw)
    = ActivityOf state0' handle' draw'
        where
            state0' = WithLevel 0 state0
            handle' s (WithLevel levelNumber state)
                = if isLevelComplete state
                  then WithLevel (levelNumber + 1)
                                 (toWorld (genLev gen
                                                  (levelNumber + 1)))
                  else WithLevel levelNumber (handle s state)
            draw' (WithLevel levelNumber state)
                = drawLevelNumber levelNumber <> (draw state)


-- | Loading level into the state
levelToState :: Level -> State
levelToState Level{..} = State { worldMap = parseMap levelMap
                                , insideButton = False
                                , openDoorsColors = openColors
                                , playerPos = initialPos
                                , playerDir = initialDir
                                , keysPressed = S.empty
                                , showMap = False
                                , energy = 100.0
                                , time = 0.0
                                }

-- | Is current level complete given some game 'State'?
isLevelComplete :: State -> Bool
isLevelComplete State{..} = exitReached worldMap playerPos

-- | Mapping object to its color
objectColor :: GameObject -> Color
objectColor Wall = gray
objectColor Floor = white
objectColor Border = RGBA 0 0 0 (0.0)
objectColor Exit = aquamarine
objectColor Entrance = red
objectColor (Door color) = color
objectColor (Button color) = color
objectColor Battery = violet

-- | Initial ActivityOf
coreActivity ::RandomGen g => g -> ActivityOf State
coreActivity gen = ActivityOf
                   (levelToState (generateLevel gen 0))
                   handle
                   render



{- MAIN -}

getLevelArr :: Level -> [String]
getLevelArr Level {..} = levelMap


getSplitNum:: Area -> Int
getSplitNum (Area (x1, y1) (x2,y2)) = y2 + 1

generateLevel :: RandomGen g => g -> Int -> Level
generateLevel gen lvlNum
    = Level {
              levelMap = (addBorders stuff),
              openColors = [],
              initialPos = (1.5,1.5),
              initialDir = (0.5, 0.5)
             }
  where
    maze = generateDifficultLevel gen lvlNum
    area = getAreaFromMaze maze
    inds = generateIndexRange area
    stuff = map joinString (splitEvery (getSplitNum area)
                                       (mazeToGrid inds maze []))


game :: IO ()
game = do
  g <- newStdGen
  runInteraction (withStartScreen (withManyLevels g
                                                  generateLevel
                                                  levelToState
                                                  isLevelComplete
                                                  (coreActivity g)))
