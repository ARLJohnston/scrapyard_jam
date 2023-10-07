{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core (clearBackground, getRenderWidth, getRenderHeight, initWindow, windowShouldClose, beginDrawing, endDrawing, getFrameTime, getKeyPressed)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (red, lightGray, darkGray)
import Raylib.Core.Shapes (drawCircle, drawRectangle)
import Raylib.Types (KeyboardKey (KeyW, KeyA, KeyS, KeyD))

import System.Random (randomRIO)
import Data.List
import GHC.Float (int2Float)

data GridState = GridState (Int, Int) [Int] [Int]

--gridSize as width height
gridSize :: (Int, Int)
gridSize = (10, 10)

randomList :: Int -> Int -> IO([Int])
randomList maxValue 0 = return []
randomList maxValue n = do
  r <- randomRIO (0, maxValue)
  rs <- randomList maxValue (n-1)
  return (r:rs)

renderPlayer :: (Int,Int) -> IO ()
renderPlayer (x,y) = do
  screenWidth <- getRenderWidth :: IO Int
  screenHeight <- getRenderHeight :: IO Int
  let cellSizeUnscaled = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))
  let coords = ((x * (fst cellSizeUnscaled) + (fst cellSizeUnscaled `div` 2)),(y * (snd cellSizeUnscaled) + (snd cellSizeUnscaled `div` 2)))
  drawCircle (fst coords) (snd coords) ((int2Float(snd cellSizeUnscaled)) * 0.4) red

renderGrid :: GridState -> IO ()
renderGrid (GridState cellSize _ []) = return ()
renderGrid (GridState cellSize [] _) = return ()
renderGrid (GridState cellSize (x:xs) (y:ys)) = do
  screenWidth <- getRenderWidth :: IO Int
  screenHeight <- getRenderHeight :: IO Int
  let cellSizeUnscaled = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))

  drawRectangle (x * (fst cellSizeUnscaled)) (y * (snd cellSizeUnscaled)) (fst cellSize) (snd cellSize) lightGray 
  renderGrid (GridState cellSize xs ys)

-- Game Of Life Logic
neighbours :: (Int, Int) -> [(Int, Int)] -> Int -> Int
neighbours (x,y) [] count = count
neighbours (x,y) ((s,t):lst) count =  do
  if (x == s && y == t) then neighbours (x,y) lst (count+1) else neighbours (x,y) lst count

survives :: (Int, Int) -> [Int] -> [Int] -> Bool
survives _ [] _ = error "no xs"
survives _ _ [] = error "no ys"
survives (x,y) xs ys = do
  let xChecks = [-1,1]
  let yChecks = [-1,1]
  let cartesian =  [(s+x,t+y) | s <- xChecks, t <- yChecks]
  let adjacent = neighbours (x,y) cartesian 0
  if (adjacent < 2 || adjacent > 3 ) then False else True

resurrects :: (Int, Int) -> [Int] -> [Int] -> Bool
resurrects _ [] _ = error "no xs"
ressurects _ _ [] = error "no ys"
ressurects (x,y) xs ys = do
  let xChecks = [-1,1]
  let yChecks = [-1,1]
  let cartesian =  [(s+x,t+y) | s <- xChecks, t <- yChecks]
  let adjacent = neighbours (x,y) cartesian 0
  if (adjacent == 3) then True else False

--gameLogic :: GridState -> GridState
--gameLogic (GridState cellSize xs ys) = do
--  let gridCoords = [(s,t) | s <- [0..(fst gridSize)], t<- [0..(snd gridSize)]]
--  let survivors = filter (survives gridCoords xs ys) gridCoords
--  let resurrectors = filter (map resurrects gridCoords xs ys) gridCoords
--  let newGrid = union survivors resurrectors
--  (GridState cellSize (map fst newGrid) (map snd newGrid))
--
--tupleMap f ((x,y):tups) l1 l2 = 
-- map (\x -> f x arg1 arg2) tuples

playerMove :: (Int,Int) -> KeyboardKey -> (Int,Int)
playerMove (x,y) key 
  | key == KeyW && (y > 0) = (x,(y-1))
  | key == KeyA && (x > 0) = ((x-1),y)
  | key == KeyS && (y < (snd gridSize)-1) = (x,(y+1))
  | key == KeyD && (x < (fst gridSize)-1) = ((x+1),y)
  | otherwise = (x,y)

gameLoop :: GridState -> (Int,Int) -> Float -> IO ()
gameLoop gridState playerPosition lastFrameTime = do
  close <- windowShouldClose :: IO Bool 
  t <- getFrameTime :: IO Float
  if close then return ()
  else do
    beginDrawing
    screenWidth <- getRenderWidth :: IO Int
    screenHeight <- getRenderHeight :: IO Int
    
    let cellSize = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))
    
    clearBackground darkGray
    
    let gridCoords = [(s,t) | s <- [0..(fst gridSize)], t<- [0..(snd gridSize)]]
    let grid = (GridState ((fst cellSize) - (fst cellSize `div` 10), (snd cellSize) - (snd cellSize `div` 10)) (map fst gridCoords) (map snd gridCoords))
    renderGrid grid

    renderPlayer playerPosition
    endDrawing

    -- deltatime type beat >>= Mutate and collisions
    movement <- getKeyPressed
    --newPlayerPosition = playerMove playerPosition movement

    gameLoop gridState (playerMove playerPosition movement) t 

main :: IO ()
main = do
  ys <- randomList (10) (snd gridSize):: IO [Int]
  xs <- randomList (10) (fst gridSize) :: IO [Int]
  let initialState = (GridState (10,10) xs ys)
  let playerPosition = (0,0)

  initWindow 600 450 "The Scrapyard"

  gameLoop initialState playerPosition 0 
