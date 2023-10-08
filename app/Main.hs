{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core (clearBackground, getRenderWidth, getRenderHeight, initWindow, windowShouldClose, beginDrawing, endDrawing, getFrameTime, getKeyPressed)
import Raylib.Core.Text (drawText, measureText)
import Raylib.Util.Colors (red, lightGray, darkGray, black, rayWhite, gray)
import Raylib.Util (whileWindowOpen0)
import Raylib.Core.Shapes (drawCircle, drawRectangle)
import Raylib.Types (KeyboardKey (KeyW, KeyA, KeyS, KeyD, KeyUp, KeyDown, KeyLeft, KeyRight, KeyNull, KeyK, KeyH, KeyJ, KeyL), Color)

import System.Random (randomRIO)
import GHC.Float (int2Float)

data GridState = GridState (Int, Int) [Int] [Int]

--gridSize as width height
gridSize :: (Int, Int)
gridSize = (20, 20)

initialNumPoints :: Int
initialNumPoints = 150

-- Initialisation
randomSpawn :: Int -> [Int] -> [Int] -> IO((Int,Int))
randomSpawn maxValue xs ys = do
  x <- randomRIO (0, maxValue-1)
  y <- randomRIO (0, maxValue-1)
  if pointExists x y xs ys
      then randomSpawn maxValue xs ys
      else return (x,y)

randomList :: Int -> Int -> IO([Int])
randomList _ 0 = return []
randomList maxValue n = do
  r <- randomRIO (0, maxValue-1)
  rs <- randomList maxValue (n-1)
  return (r:rs)

-- Rendering
renderPlayer :: (Int,Int) -> IO ()
renderPlayer (x,y) = do
  screenWidth <- getRenderWidth :: IO Int
  screenHeight <- getRenderHeight :: IO Int
  let cellSizeUnscaled = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))
  let coords = ((x * (fst cellSizeUnscaled) + (fst cellSizeUnscaled `div` 2)),(y * (snd cellSizeUnscaled) + (snd cellSizeUnscaled `div` 2)))
  drawCircle (fst coords) (snd coords) ((int2Float(snd cellSizeUnscaled)) * 0.4) red

renderGrid :: GridState -> Color -> IO ()
renderGrid (GridState _ _ []) _ = return ()
renderGrid (GridState _ [] _) _ = return ()
renderGrid (GridState cellSize (x:xs) (y:ys)) color = do
  screenWidth <- getRenderWidth :: IO Int
  screenHeight <- getRenderHeight :: IO Int
  let cellSizeUnscaled = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))

  drawRectangle (x * (fst cellSizeUnscaled)) (y * (snd cellSizeUnscaled)) (fst cellSize) (snd cellSize) color 
  renderGrid (GridState cellSize xs ys) color

-- Game Of Life Logic
survives :: (Int, Int) -> [Int] -> [Int] -> Bool
survives _ [] _ = False
survives _ _ [] = False
survives (x, y) xs ys = do
  let xChecks = [-1,1]
  let yChecks = [-1,1]
  let cartesian = [(s + x, t + y) | s <- xChecks, t <- yChecks]
  let aliveNeighbors = filter (\(s, t) -> pointExists s t xs ys) cartesian
  let neighborCount = length aliveNeighbors
  case neighborCount of
    2 -> True
    3 -> True
    _ -> False

pointExists :: Int -> Int -> [Int] -> [Int] -> Bool
pointExists _ _ [] _ = False
pointExists _ _ _ [] = False
pointExists x y (s:xs) (t:ys) = do
  if (x == s && y == t) then True else pointExists x y xs ys

-- Logic for Gameplay
gameLogic :: GridState -> GridState
gameLogic (GridState cellSize xs ys) = do
  let gridCoords = [(s,t) | s <- [0..(fst gridSize)], t<- [0..(snd gridSize)]]
  let survivors = filter (\point -> survives point xs ys) gridCoords
  (GridState cellSize (map fst survivors) (map snd survivors))

playerMove :: (Int,Int) -> KeyboardKey -> (Int,Int)
playerMove (x,y) key 
  | (key == KeyW || key == KeyK ||key == KeyUp) && (y > 0) = (x,(y-1))
  | (key == KeyA || key == KeyH ||key == KeyLeft) && (x > 0) = ((x-1),y)
  | (key == KeyS || key == KeyJ ||key == KeyDown) && (y < (snd gridSize)-1) = (x,(y+1))
  | (key == KeyD || key == KeyL ||key == KeyRight) && (x < (fst gridSize)-1) = ((x+1),y)
  | otherwise = (x,y)

scaleGrid :: GridState -> (Int,Int) -> GridState
scaleGrid (GridState _ xs ys) (xScale, yScale) = (GridState (xScale,yScale) xs ys)

collision :: GridState -> (Int,Int) -> Bool
collision (GridState _ [] _) (_,_) = False
collision (GridState _ _ []) (_,_) = False
collision (GridState c (x:xs) (y:ys)) (xPlayer,yPlayer) = if (x == xPlayer && y == yPlayer) then True else collision (GridState c xs ys) (xPlayer, yPlayer)

gameLoop :: GridState -> (Int,Int) -> Float -> Float -> Bool -> IO (Float)
gameLoop gridState playerPosition lastFrameTime score dead = do
  close <- windowShouldClose :: IO Bool 
  t <- getFrameTime :: IO Float
  let time = lastFrameTime + t
  if (close || dead) then return (score)
  else do
    beginDrawing
    screenWidth <- getRenderWidth :: IO Int
    screenHeight <- getRenderHeight :: IO Int
    
    let cellSize = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))
    
    clearBackground darkGray
    
    let gridCoords = [(xCoord,yCoord) | xCoord <- [0..(fst gridSize)], yCoord<- [0..(snd gridSize)]]
    let newGridSize = ((fst cellSize) - (fst cellSize `div` 10), (snd cellSize) - (snd cellSize `div` 10))
    let grid = (GridState newGridSize (map fst gridCoords) (map snd gridCoords))
    let indicator = gameLogic gridState
    renderGrid grid lightGray
    renderGrid gridState black
    renderGrid indicator gray

    renderPlayer playerPosition
    drawText ("Score:" ++ (show (floor score :: Int)) ) 0 0 20 rayWhite
    endDrawing

    movement <- getKeyPressed

    let newState = if (time > 1)
        then (gameLogic gridState)
        else gridState

    --let newScore = if (time > 1)
    --    then (score+100)
    --    else score

    let newTime = if (time > 1)
        then (time-1)
        else time
    let newScore = (score + (time / 100))

    gameLoop (scaleGrid (newState) newGridSize) (playerMove playerPosition movement) newTime newScore (collision gridState playerPosition)

titleScreen :: Bool -> IO ()
titleScreen adv = do
  close <- windowShouldClose :: IO Bool
  if (close || adv) then return ()
  else do
    key <- getKeyPressed :: IO KeyboardKey


    beginDrawing
    screenWidth <- getRenderWidth :: IO Int
    screenHeight <- getRenderHeight :: IO Int
    clearBackground lightGray
    titleWidth <- measureText "Conway's Game of Life & Death" 35 :: IO Int
    subWidth <- measureText "Press the 'any' key" 18 :: IO Int
    drawText "Conway's Game of Life & Death" (screenWidth `div` 2 - (titleWidth `div` 2)) ((screenHeight `div` 2) - 20) 35 black
    drawText "Press the 'any' key" (screenWidth `div` 2 - (subWidth `div` 2)) ((screenHeight `div` 2) + 40) 18 darkGray 
    endDrawing

    titleScreen (key /= KeyNull) 


gameOverScreen :: Float -> Bool -> IO ()
gameOverScreen score adv = do
  close <- windowShouldClose :: IO Bool
  if (close || adv) then return ()
  else do
    key <- getKeyPressed :: IO KeyboardKey


    beginDrawing
    clearBackground darkGray
    screenWidth <- getRenderWidth :: IO Int
    screenHeight <- getRenderHeight :: IO Int
    width <- measureText "YOU DIED" 80 :: IO Int
    subWidth <- measureText ("Score:" ++ (show (floor score :: Int)) ) 20 :: IO Int
    drawText "YOU DIED" (screenWidth `div` 2 - (width `div` 2)) ((screenHeight `div` 2) - 20) 80 red
    drawText ("Score:" ++ (show (floor score :: Int)) ) (screenWidth `div` 2 - (subWidth `div` 2)) ((screenHeight `div` 2) + 120) 20 rayWhite
    endDrawing

    gameOverScreen score (key /= KeyNull) 

main :: IO ()
main = do
  ys <- randomList (snd gridSize) initialNumPoints :: IO [Int]
  xs <- randomList (fst gridSize) initialNumPoints :: IO [Int]

  screenWidth <- getRenderWidth :: IO Int
  screenHeight <- getRenderHeight :: IO Int
    
  let cellSize = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))
  let initialState = (GridState ((fst cellSize) - (fst cellSize `div` 10), (snd cellSize) - (snd cellSize `div` 10)) xs ys)
  playerPosition <- randomSpawn (fst gridSize) xs ys :: IO (Int,Int)

  _ <- initWindow 600 450 "Conway's Game of Life & Death"

  whileWindowOpen0
    ( do
    close <- windowShouldClose :: IO Bool
    if close 
        then return ()
        else do
          titleScreen False
          score <- (gameLoop initialState playerPosition 0 0 False) :: IO Float
          gameOverScreen score False
    )
